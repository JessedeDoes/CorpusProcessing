package posmapping

import java.io.{File, PrintWriter}

import corpusprocessing.CGN.CGNToTEIWithTDNTags
import corpusprocessing.CGNMiddleDutch
import corpusprocessing.gysseling.HistoricalTagsetPatching

import scala.xml.XML
object DataSettings {
   val onwDir = "/home/jesse/workspace/data-historische-corpora/ONW/ONW-processed-metadata/"
   val gysDir = "../data-historische-corpora/gysseling/gysseling-processed-metadata/"
}

object TagsetDiachroonNederlands {
  val TDN_xml = "data/TDN/TDN_tagset.xml"
  lazy val TDNTagset = TagSet.fromXML(TDN_xml)
  val stylesheet = "/home/jesse/workspace/xml2rdf/src/main/scala/posmapping/tagset-documentation.xsl"

  def integratedTag(tag: String) = CHNStyleTag(tag, TDNTagset)
  //case class IntegratedTag(tag: String) extends CHNStyleTag(tag, TDNTagset)

  def printTagsetXML(tagset: TagSet, corpusName: String, pw: PrintWriter = new PrintWriter(System.out)): Unit = {
    //val xmlWriter = new PrintWriter(System.out)
    pw.println(TagSet.pretty.format(tagset.toXML(corpus=corpusName)))
    pw.close()
  }

  /*
   lazy val features_sorted = if (tagset == null) features else {
    val pos = features.find(_.name == "pos").map(_.value).getOrElse("UNK")
    val lijstje: Map[String, Int] = tagset.pos2partitions.getOrElse(pos, List()).zipWithIndex.toMap
    val additions = features.filter(f => !lijstje.contains(f.name)).zipWithIndex.map({case (x,i) => (x.name,lijstje.size + i)}).toMap
    def combi = lijstje ++ additions
    println(combi)
    features.sortBy(x => combi(x.name))
  }

   */

  def sortFeatures(t: TagSet, tRef: TagSet): TagSet = {

    def sortPartition(pos: String, features: List[String]) = {
      val lijstje: Map[String, Int] = tRef.pos2partitions.getOrElse(pos, List()).zipWithIndex.toMap

      // dit sorteert gecombineerde waarden nog niet goed.....

      val additions = features.filter(f => !lijstje.contains(f)).zipWithIndex.map({case (x,i) => (x,lijstje.size + i)}).toMap

      val combi = lijstje ++ additions

      (pos, features.sortBy(x => combi(x)))
    }

    def sortFeatures(n: String, v: List[String]) =
    {
      val lijstje: Map[String, Int] = tRef.partitions.getOrElse(n, List()).zipWithIndex.toMap
      val addition  = v.filter(f => !lijstje.contains(f)).zipWithIndex.map({case (x,i) => (x,lijstje.size + i)}).toMap
      val combi = lijstje ++ addition
      v.sortBy(x => combi(x)).map(tRef.normalizeFeatureValue(n,_))
    }

    def sortValueRestriction(vr: ValueRestriction) = vr.copy(values = vr.values.map(tRef.normalizeFeatureValue(vr.featureName,_)))
    t.copy(
      partitions = t.partitions.map({case (n,v)=> (n,sortFeatures(n,v))}),
      pos2partitions = t.pos2partitions.map({case (p,f)=> sortPartition(p,f)}),
      valueRestrictions = t.valueRestrictions.map(sortValueRestriction))
  }


  def addDescriptionsFromTDN(prefix: String, corpusBased: TagSet, outputName: String = "tagset_desc",
                             corpusName: String, corpusNameInXML: Option[String] = None): TagSet = {

    // val corpusBased = TagSet.fromXML(prefix + fileName) // nee, niet goed


    val corpusBasedWithDesc0 = corpusBased.copy(
      descriptions = TDNTagset.descriptions,
      displayNames = TagSet.mergeDescriptions(TDNTagset.displayNames,
        TDNTagset.descriptions),
      implications = TDNTagset.implications.filter(x =>
        x.corpus.isEmpty || x.corpus.get.contains(corpusName)))

    // wat hier nog misgaat is dat door implicatie toegevoegde features niet doorkomen......
    // je moet eigenlijk eerst fixTags doen, en DAN weer opnieuw de hele zaak draaien......

    val corpusBasedWithDesc = sortFeatures(corpusBasedWithDesc0, TDNTagset).fixAllTags()

    TagSet.compare(TDNTagset, corpusBasedWithDesc)

    val outputBase = prefix + outputName
    printTagsetXML(corpusBasedWithDesc, corpusNameInXML.getOrElse(corpusName), new PrintWriter(s"$outputBase.xml"))

    val jsonWriter = new PrintWriter(s"$outputBase.json")
    jsonWriter.println(corpusBasedWithDesc.asJSON)
    jsonWriter.close()

    val blfWriter = new PrintWriter(s"$outputBase.blf.yaml")
    blfWriter.println(corpusBasedWithDesc.forBlacklab)
    blfWriter.close()

    val z = new File(stylesheet)
    if (z.exists) {
      val x = new utils.XSLT(stylesheet)
      x.transform(s"$outputBase.xml", s"$outputBase.html")
      x.transform(inFile = TDN_xml, outFile = TDN_xml.replaceAll("xml", "html"))
    }
    //compareGysselingToMolex.pretty(molexWithDesc, new PrintWriter("/tmp/molex_tagset_displayNames.xml"))
    corpusBasedWithDesc
  }

  def tagsetFromCorpusFiles(dirName: String, attribute: String, prefix: String = "/tmp/", corpus: String,
                            separator: String = "[+]") = { // let op BaB heeft | tussen tags, dat kan misgaan
    val dir = new File(dirName)

     val files: Set[File] = if (dir.isDirectory) dir.listFiles().toSet else Set(dir)

     val allDistinctTags: Set[String] = files.flatMap(
      f => {
        scala.util.Try(
          {
            val doc = XML.loadFile(f)

            val combiposjes = (doc \\ "w").map(x => (x \ s"@$attribute").text).toSet
            val posjes = combiposjes.flatMap(p => p.split(separator).toSet)
            //println(posjes)
            posjes
          }) match {
          case scala.util.Success(x) => x
          case _ => Set[String]()
        }
      }
    )

    tagsetFromSetOfTags(prefix, corpus, allDistinctTags)
  }

  implicit def setToMap(s: Set[String]): Map[String, String] = s.toIndexedSeq.zipWithIndex.map({case (x,i) => i.toString -> x}).toMap

  def ambiPatch(m: Map[String,String]) = {
    val m1 = m.mapValues(t => t.split("\\)[|+]")).mapValues(r => r.zipWithIndex.map({case (t,i) => if (i == r.size-1) t else t + ")"}))
    m1.flatMap({case (k,a) => a.zipWithIndex.map({case (v,i) => if (i==0) k->v else s"$k[$i]" -> v})})
  }

  def tagsetFromSetOfTags(prefix: String, corpus: String, tagMapping_0: Map[String, String], corpusNameInXML: Option[String] = None) = {

    val tagMapping = ambiPatch(tagMapping_0)

    val allDistinctTags = tagMapping.values.toSet

    val listWriter = new PrintWriter(prefix + "tagList.txt")
    allDistinctTags.toList.sorted.foreach(t => listWriter.println(t))
    listWriter.close()

    val tagset = CHNStyleTags.tagsetFromSetOfStrings("pos", allDistinctTags.flatMap(t => t.split("\\s*\\+\\s*").toSet))

    val xmlWriter = new PrintWriter(prefix + "tagset.xml")

    printTagsetXML(tagset, corpus, xmlWriter)
    xmlWriter.close()

    val jsonWriter = new PrintWriter(prefix + "tagset.json")
    jsonWriter.println(tagset.asJSON)
    jsonWriter.close()

    val blfWriter = new PrintWriter(prefix + "tagset.blf.yaml")
    blfWriter.println(tagset.forBlacklab)
    blfWriter.close()

    val tagsetPlus = addDescriptionsFromTDN(prefix, tagset, "tagset_desc_temp", corpus, corpusNameInXML)

    val tmPlus = tagMapping.mapValues(t => {
      val t1 = new CHNStyleTag(t, tagsetPlus)
      val t1Fixed = tagsetPlus.fixTag(t1).toString
      (t,t1Fixed)
    })

    val currentMapping = new PrintWriter(prefix + "tagset.mapped.txt")

    tmPlus.toList.sortBy(_._2._2).foreach({case (tOrg, (t,t1)) => {
      currentMapping.println(s"$tOrg\t$t\t$t1")
    }})
    currentMapping.close()

    val currentMappingHTML =  <html> <table>{tmPlus.toList.sortBy(_._2._2).map({case (tOrg, (t,t1))  => {
      <tr><td>{tOrg}</td><td>{t1}</td></tr>
    }})}</table></html>

    XML.save(prefix + "tagset.mapped.html", currentMappingHTML, "UTF-8")

    val r = tagMapping.mapValues(s => {
      val t0 = new CHNStyleTag(s, tagsetPlus)
      val t1 = tagsetPlus.fixTag(t0)
      t1.asInstanceOf[CHNStyleTag]}
    )
    r.toList.sortBy({case (x,t) => t.toString()}).foreach({case (x, t) =>
        val (ok, infringed) = t.tagset.tagSatisfiesImplications(t)
        val redundancies = if (ok) t.hasUnforcedFeatures() else List[(String,Tag)]();
        if (!ok)
        {
          Console.err.println(s"$x $t implications: $ok Infringed: ($infringed)")
        } else if (redundancies.nonEmpty)
        {
          Console.err.println(s"$x $t  Redundant: ${redundancies.map(_._1)}")
        }
    })
    r.mapValues(_.removeUnforcedFeatures())
  }

  def doONW = {
      // tagsetFromCorpusFiles(DataSettings.onwDir, "msd", "data/TDN/Corpora/ONW/", "ONW")

    val conversionTable = "/tmp/allTags.onwmap.txt"

    val mapping = io.Source.fromFile(conversionTable).getLines().map(_.split("\\t")).map(r => s"${r(0)};${r(1)}" -> r(2)).toMap
    tagsetFromSetOfTags("data/TDN/Corpora/ONW/", "ONW", mapping)
   }

  def doGysseling = {
     // tagsetFromCorpusFiles(DataSettings.gysDir, "pos", "data/TDN/Corpora/Gysseling/", "gysseling_nt")
    val mappingFile = "/mnt/Projecten/CLARIAH/CLARIAH-PLUS/Wp3/HistoricalDutch/Literature/Drive/gysseling.tsv"

    lazy val mapping = io.Source.fromFile(mappingFile).getLines.map(l => {
      val x = l.split("\\t")
      x(0) -> x(1)
    }).toMap

    lazy val descriptions = io.Source.fromFile(mappingFile).getLines.map(l => {
      val x = l.split("\\t")
      x(0) -> x(2)
    }).toMap

    val m = tagsetFromSetOfTags("data/TDN/Corpora/Gysseling/", "Gysseling", mapping, Some("gysseling_nt"))

    val mappingRows = m.toList.sortBy(_._1).map({case (c, t) => <tr><td>{c}</td><td>{t.toString}</td><td>{descriptions.getOrElse(c, "")}</td></tr>})
    val mappingHTML = <table>{mappingRows}</table>

    XML.save("data/TDN/Corpora/Gysseling/current_mapping.html", mappingHTML, "UTF-8")
    m
  }

  val CGnMainMap = Map(
    "N.*soort.*" -> "NOU-C",
    "N.*eigen.*" -> "NOU-P",
  )

  def doCGN = {
    //folia.TDNTest.main(Array())

    val mappingFile = CGNToTEIWithTDNTags.mappingFile

    lazy val descriptions = io.Source.fromFile(mappingFile).getLines.map(l => {
      val x = l.split("\\t")

      val d = if (x.size >3) x(3) else ""
      x(1) -> d
    }).toMap



    val m = tagsetFromSetOfTags("data/TDN/Corpora/CGN/", "CGN", CGNToTEIWithTDNTags.mapping, Some("CGN_TDN"))

    val mappingRows = m.toList.sortBy(_._2.toString).map({case (c, t) =>
      val p1 = c.replaceAll("\\(.*", "")
      val p = t.pos
      val color = if (p == p1) "white" else "orange"
      val style = s"background-color: $color"
      <tr><td>{c}</td><td>{t.toString}</td><td>{descriptions.getOrElse(c, "")}</td></tr>})

    val mappingHTML = <table>{mappingRows}</table>

    XML.save("data/TDN/Corpora/CGN/current_mapping.html", mappingHTML, "UTF-8")

    m
    // tagsetFromCorpusFiles("/home/jesse/Links/Werkfolder/Projecten/InterviewsCGN/TEI", "pos", "data/TDN/Corpora/CGN/", "CGN")
  }

   def main(args: Array[String]): Unit = {
     doCGN
     doGysseling
     doONW
   }
}

/*
def oldMain(args: Array[String]): Unit = {
    val GysTags = scala.io.Source.fromFile("data/CG/overzichtje_met_morfcode.txt").getLines.map(l => l.split("\\s+")) // .map(l => l(2))

    val patchedTagset = new PrintWriter("/tmp/gystags.out")

    GysTags.foreach(ts => {
      val (m, t1, t) = (ts(0), ts(1), ts(2))
      val restje = ts.drop(3).mkString(" ")
      val tPatched = HistoricalTagsetPatching.patchPoSMistakes(m, t1, "", "")
      val tag = IntegratedTag(tPatched)
      val v = TDNTagset.isValid(tag)
      patchedTagset.println(s"${ts(0)}\t$tPatched\t$restje)")
    })

    patchedTagset.close()
  }
 */

