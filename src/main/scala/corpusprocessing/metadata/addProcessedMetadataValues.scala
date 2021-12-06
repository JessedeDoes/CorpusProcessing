package corpusprocessing.metadata

import corpusprocessing.onw.{Settings, onwCorpus}
import database.DatabaseUtilities.AlmostQuery
import utils.PostProcessXML
import utils.PostProcessXML._

import scala.collection.immutable
import scala.xml._

case class addProcessedMetadataValues() {
  def getId(n: Node):Option[String] = n.attributes.filter(a => a.prefixedKey.endsWith(":id") || a.key=="id").map(_.value.toString).headOption

  val nice = false

  def addField(b: Elem, n: String, v0: Seq[String]): Elem = {
    Console.err.println(s"##### $n -> $v0")
    val v = if (n.matches(".*(genre|fict).*")) v0.map(_.toLowerCase) else v0
    if (v.isEmpty) b else {
      val b1 = b.copy(child = b.child ++ Seq(
        <interpGrp type={n}>
          {v.map(x => <interp>{x}</interp>)}
        </interpGrp>
      ))
      val check = getField(b1, n)
      Console.err.println(s"##### CHECK $n -> $check")
      b1
    }
  }

  def removeField(b: Elem, n: String) =
    PostProcessXML.updateElement5(b, e => e.label=="interpGrp" && (e \ "@type").text== n, x => Seq()).asInstanceOf[Elem]

  def updateField(b: Elem, n: String, v0: Seq[String])  = {
    addField(removeField(b,n), n, v0)
  }

  def getFieldMap(b: Elem): Map[String, immutable.Seq[String]] = {
    val map = (b \ "interpGrp").map(g =>
      (g \ "@type").text -> (g \ "interp").map(_.text).filter(_.nonEmpty)
    ).toMap
    map
  }

  def getFieldFunction(b: Elem): String => Seq[String] = {
    val map = (b \ "interpGrp").map(g =>
      (g \ "@type").text -> (g \ "interp").map(_.text).filter(_.nonEmpty)
    ).toMap
    field => map.getOrElse(field, Seq())
  }

  def getField(b: Elem, n: String): immutable.Seq[String] = {
    (b \ "interpGrp").filter(g => (g \ "@type").text == n).flatMap(g => (g \ "interp").map(_.text))
  }

  def getFields(b: Elem, n: String => Boolean = x => true): immutable.Seq[(String,String)] = {
    (b \ "interpGrp").filter(g => n((g \ "@type").text)).flatMap(g => (g \ "interp").map(
      x => (g \ "@type").text -> x.text)).filter(_._2.nonEmpty)
  }

  def addProcessedValue(b: Elem, n: String, f: Seq[String]): Elem = {
     addField(b, n, f)
  }

  def coalesce(m: String => Seq[String], fields: Seq[String]): Seq[String] =
    fields.foldLeft(Seq[String]())({case (b,v) => if (b.nonEmpty) b else m(v)})

  def eentwee(s:String) = List(s + "Level0", s + "Level1", s + "Level2")
  def eentweeloc(s:String) = {
    val s1 = s.replaceAll("witnessLoc","loc")
    List(s + "Level0", s + "Level1", s + "Level2", s1 + "Level0", s1 + "Level1", s1 + "Level2")
  }

  def moveHistory(b: Elem) = {

    val history = ((b \ "interpGrp").filter(g => (g \ "@type").text.startsWith("genre"))).map(
      g => (g \ "@type").text -> g.text.toLowerCase.contains("geschiedenis")).filter(_._2).map(_._1).map(_.replaceAll("genre","subgenre"))

    val addProse = if (history.nonEmpty) Seq(<interp>prose</interp>) else Seq()
    Console.err.println("history fields: " + history)

    val cleanedChildren = b.child.map(g => {
      if (g.label != "interpGrp" || !(g \ "@type").text.startsWith("genre")) g else {
        val terpjes = g.child.filter(_.text != "geschiedenis")
        g.asInstanceOf[Elem].copy(child = terpjes ++ addProse)
      }
    })

    val patchHistory = history.filter(h => (b \ "interpGrp").filter(g => (g \ "@type").text == h).nonEmpty)
    val addHistory = history diff patchHistory

    val hist = <interp>history</interp>

    val enhancedChildren = cleanedChildren.map(
      x =>  {
        val t = (x \ "@type").text
        if (history.contains(t)) x.asInstanceOf[Elem].copy(child = x.child ++ Seq(hist)) else x
      }
    ) ++ addHistory.map(x => <interpGrp type={x}>{hist}</interpGrp>)
    Console.err.println((enhancedChildren \\ "interpGrp").filter(z => z.text.contains("history") || z.text.contains("geschiedenis")))
    b.copy(child = enhancedChildren)
  }

  def renameFactuality(b: Elem) = {

    val sourceID = getField(b,"sourceID").head

    def rename(ig: Elem) = {
      val t = (ig \ "@type").text.replaceAll("factuality","fictionality")
      val value = ig.text.replaceAll("ctie", "ction").toLowerCase.trim
      val z = <interpGrp type={t}><interp>{value}</interp></interpGrp>
      Console.err.println(s"FACT OR FICTION! $sourceID --> $z")
      z
    }

    if (sourceID.matches("corpusgysseling.[01].*")) PostProcessXML.updateElement(b, e => (e \ "@type").text.contains("fact"), rename) else b
  }

  lazy val addedFieldNames = List("witness_year_from",
    "witness_year_to",
    "text_year_from" ,
    "text_year_to" ,
    "datering" ,
    "decade" ,
    "country" ,
    "region" ,
    "province" ,
    "place" ,
    "kloeke" ,
    "genre" ,
    "subgenre" ,
    "title" ,
    "subtitle" ,
    "author",
    "Subcorpus")

  def enrichBibl(b0: Elem): Elem = {

    val b = renameFactuality(moveHistory(b0))

    val map: String => Seq[String] = getFieldFunction(b)

    val titleLevel2 = map("titleLevel2")
    val titleLevel1 = map("titleLevel1")
    val subtitleLevel2 = map("subtitleLevel2")

    val eh_subcorpus = if (titleLevel2.nonEmpty && subtitleLevel2.nonEmpty) {
      if (titleLevel1.exists(x => x.startsWith("gesproken-brief"))) Seq("CGTL1 (gesproken taal 1)") else Seq(titleLevel2.head + " (" + subtitleLevel2.head + ")")
    } else
      Seq()


    val genre: Seq[String] = coalesce(map, eentwee("genre")).map(_.toLowerCase)
    val subgenre = coalesce(map, eentwee("subgenre"))
    val title = coalesce(map, eentwee("title"))
    val subtitle = coalesce(map, eentwee("subtitle"))

    val witness_year_from = coalesce(map, List("witnessYearLevel1_from", "witnessYearLevel2_from"))
    val witness_year_to = coalesce(map, List("witnessYearLevel1_to", "witnessYearLevel2_to"))
    val author = coalesce(map, eentwee("author")).filter(x => x.trim != "J. van Gorp") // AHEM zeer gruwelijk....

    val text_year_from = coalesce(map, List("textYearLevel1_from", "textYearLevel2_from"))
    val text_year_to = coalesce(map, List("textYearLevel1_to", "textYearLevel2_to"))

    val country = coalesce(map, eentweeloc("witnessLocalization_country"))
    val region = coalesce(map, eentweeloc("witnessLocalization_region"))
    val place = coalesce(map, eentweeloc("witnessLocalization_place"))
    val kloeke = coalesce(map, eentweeloc("witnessLocalization_kloekecode"))
    val province: Seq[String] = coalesce(map, eentweeloc("witnessLocalization_province")).map(_.replaceAll("west-vlaanderen", "West-Vlaanderen"))

    val average = Math.floor((witness_year_from.headOption.map(_.toDouble).getOrElse(Double.NaN) + witness_year_to.headOption.map(_.toDouble).getOrElse(Double.NaN)) / 2).toInt

    val decade: Seq[String] = Seq((average - (average % 10)).toString)
    val datering: Seq[String] = Seq(witness_year_from.headOption.getOrElse("?")  + "-"  + witness_year_to.headOption.getOrElse("?"))

    val toAdd: Seq[(String, Seq[String])] =
      List("witness_year_from" -> witness_year_from,
           "witness_year_to" -> witness_year_to,
           "text_year_from" -> text_year_from,
           "text_year_to" -> text_year_to,
           "datering" -> datering,
           "decade" -> decade,
           "country" -> country,
           "region" -> region,
           "province" -> province,
           "place" -> place,
           "kloeke" -> kloeke,
           "genre" -> genre,
           "subgenre" -> subgenre,
           "title" -> title,
           "subtitle" -> subtitle,
           "author" -> author,
           "Subcorpus" -> eh_subcorpus)

    val bNew = toAdd.foldLeft(b)( {case (b1,(n,v:Seq[String])) =>
      {
        Console.err.println(s"$n=$v")
        addProcessedValue(b1,n,v)
      } }
    )
    //val check = getFieldMap(bNew)
    //Console.err.println(check)
    bNew
  }

  def findListBibl(d: Elem) = (d \\ "listBibl").filter(l => (getId(l).map(x => x.toLowerCase.contains("inlmetadata")
    && !x.contains("Level0_citaat-id")) == Some(true)))


  def addProcessedMetadataValues(d: Elem) = {
    PostProcessXML.updateElement(d,
      l => l.label == "listBibl" &&
        (getId(l).map(x => x.toLowerCase.contains("inlmetadata")
          && !x.contains("Level0_citaat-id")) == Some(true)),
      l => updateElement(l, _.label == "bibl", enrichBibl)
    )
  }

  def fixFile(in: String, out: String) =
  {
    val d = addProcessedMetadataValues(XML.load(in))

    if (this.nice) {
      val p = new scala.xml.PrettyPrinter(300, 4)
      val t = p.format(d)
      val w = new java.io.PrintWriter(out)
      w.write(t)
      w.close()
    } else XML.save(out, d,  enc="UTF-8")
  }
}


object addRolandsCorrectionsONW extends addProcessedMetadataValues() {
  lazy val db = HistoricalMetadatabase.db
  val onwDir = HistoricalMetadatabase.ONWMetaDir
  lazy val getTekstsoort: Map[String, Map[String, String]] = db.slurp(db.allRecords("tekstsoort")).groupBy(m => m("persistentId")).mapValues(_.head)
  lazy val getAllMeta:  Map[String, Map[String, String]] = db.slurp(db.allRecords("metadata")).groupBy(m => m("persistentId")).mapValues(_.head)
  override def fixFile(in: String, out: String) =
  {
    val d0 = XML.load(in)
    val bibl = (d0 \\ "bibl").head.asInstanceOf[Elem]
    //println(bibl)
    val metaOrg = getFieldMap(bibl)
    //println(metaOrg)
    val pid = metaOrg("pid").head

    val tekstsoort: Map[String, String] = getTekstsoort(pid)
    val allMeta = getAllMeta(pid)
    val bNew = tekstsoort.foldLeft(bibl){case (b, (k,v)) => updateField(b, k, Seq(v))}
    val d1 = PostProcessXML.updateElement(d0, _ == bibl, x => bNew)
    XML.save(out, d1,  enc="UTF-8")
    /*
    if (this.nice) {
      val p = new scala.xml.PrettyPrinter(300, 4)
      val t = p.format(d)
      val w = new java.io.PrintWriter(out)
      w.write(t)
      w.close()
    } else XML.save(out, d,  enc="UTF-8")
    */

  }

  def main(args: Array[String]): Unit = {
     utils.ProcessFolder.processFolder(new java.io.File("/mnt/Projecten/Corpora/Historische_Corpora/ONW/ONW-tagsplit"),
       new java.io.File("/mnt/Projecten/Corpora/Historische_Corpora/ONW/ONW-correcties-metadata-roland"), fixFile)
     utils.ProcessFolder.processFolder(new java.io.File("/mnt/Projecten/Corpora/Historische_Corpora/ONW/ONW-correcties-metadata-roland"),
      new java.io.File("/mnt/Projecten/Corpora/Historische_Corpora/ONW/ONW-processed-metadata-v3"), addProcessedMetadataValuesONW.fixFile )
  }
}

object addProcessedMetadataValuesONW extends addProcessedMetadataValues()
{
  override val nice = false // geeft lelijke spaties in de woorden, dus moet helaas weg...
  def main(args: Array[String]): Unit = {

    import utils.ProcessFolder

    ProcessFolder.processFolder(new java.io.File(Settings.tagsplitTargetDir), new java.io.File(Settings.processedMetadataDir), fixFile )

  }
}

object enNuGysseling extends addProcessedMetadataValues() {
  def main(args: Array[String]): Unit = {

    import utils.ProcessFolder
    ProcessFolder.processFolder(new java.io.File(Settings.gysselingEnhancedTEI),
      new java.io.File(Settings.gysselingProcessedMetadataDir), fixFile )
  }
}

object enNuCRM extends addProcessedMetadataValues() {
  def main(args: Array[String]): Unit = {

    import utils.ProcessFolder
    ProcessFolder.processFolder(new java.io.File(Settings.CRMtagmappedDir),
      new java.io.File(Settings.CRMpostProcessedDir), fixFile )
  }
}

object enDanEindhoven extends addProcessedMetadataValues() {
  def main(args: Array[String]): Unit = {
    import utils.ProcessFolder
    ProcessFolder.processFolder(new java.io.File(Settings.eindhovenEnhancedTEI), new java.io.File(Settings.eindhovenProcessedMetadataDir), fixFile )
  }
}



object jamaarCLVN extends addProcessedMetadataValues() {
  def main(args: Array[String]): Unit = {
    import utils.ProcessFolder

    def putPosInPos(w: Elem) = {
      val pos = (w \ "@type").text
      val newAtts = w.attributes.filter(_.key != "type").append(new UnprefixedAttribute("pos", pos, Null))
      w.copy(attributes = newAtts)
    }
    def fixFile(in: String, out: String) =
    {
      val d0 = XML.load(in)
      val d1 = onwCorpus.wrapWordContent(d0)
      val d = PostProcessXML.updateElement(addProcessedMetadataValues(d1), _.label == "w", putPosInPos)

      if (this.nice) {
        val p = new scala.xml.PrettyPrinter(300, 4)
        val t = p.format(d)
        val w = new java.io.PrintWriter(out)
        w.write(t)
        w.close()
      } else XML.save(out, d,  enc="UTF-8")
    }

    ProcessFolder.processFolder(new java.io.File(Settings.clvnTagged),
      new java.io.File(Settings.clvnPostProcessed), fixFile )
  }
}


/*
/mnt/Projecten/Corpora/Historische_Corpora/CLVN/PostProcessedMetadata
 */
object enDanMNL extends addProcessedMetadataValues() {

  def cleanSeg(w1: Elem) = {
    val newChildren = w1.child.map(c => c match {
      case e: Elem if e.label == "hi" => e.copy(label="expan", attributes = e.attributes.filter(x => false))
      case e: Elem => e
      case t: Text => Text(t.text.trim.replaceAll("\\s+", " "))
    })
    val w2 = w1.copy(child = newChildren)
    w2
  }

  def noInterp(w: Elem) =  {
    val newChild = w.child.filter(c => c.label != "interpGrp")
    val w1 = w.copy(
      child = <seg>{newChild}</seg>,
      attributes=w.attributes.filter(a => a.key != "lemma" && a.key != "type"))

    val w2 = PostProcessXML.updateElement(w1, _.label=="seg", cleanSeg)
    val w3 = w2.copy(child = w2.child.map(c => c match {
      case t: Text => Text(t.text.trim.replaceAll("\\s+", " "))
      case _ => c
    }))
    // println(w3)
    w3
  }

  override def fixFile(in: String, out: String) =
  {
    val d0 = XML.load(in)
    val d1 = PostProcessXML.updateElement(d0, e => e.label=="w" || e.label == "pc", noInterp)
    val d = addProcessedMetadataValues(d1)

    if (this.nice) {
      val p = new scala.xml.PrettyPrinter(300, 4)
      val t = p.format(d)
      val w = new java.io.PrintWriter(out)
      w.write(t)
      w.close()
    } else XML.save(out, d,  enc="UTF-8")
  }

  def main(args: Array[String]): Unit = {
    import utils.ProcessFolder
    ProcessFolder.processFolder(new java.io.File(Settings.mnlTokenizedTEI), new java.io.File(Settings.mnlProcessedMetadataDir), fixFile )
  }
}