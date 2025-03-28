package posmapping

import java.io.{File, PrintWriter}
import corpusprocessing.CGN.CGNToTEIWithTDNTags
import corpusprocessing.CGNMiddleDutch
import corpusprocessing.gysseling.HistoricalTagsetPatching
import posmapping.TagsetDiachroonNederlands.{TDN_core_xml, TDN_xml}
import utils.XSLT

import scala.xml.XML
object DataSettings {
   val onwDirOld = "/mnt/Projecten/Corpora/Historische_Corpora/ONW/ONW-processed-metadata-v3" // /home/jesse/workspace/data-historische-corpora/ONW/ONW-processed-metadata/"
   val onwDir = "/mnt/Projecten/Corpora/Historische_Corpora/ONW/ONW-januari-2022"
   val gysDir = "../data-historische-corpora/gysseling/gysseling-processed-metadata/"
   val babDir="/mnt/Projecten/corpora/Historische_Corpora/BrievenAlsBuit/2.8TDN"
}

object TagsetDiachroonNederlands {
  val TDN_xml = "data/TDN/TDN_tagset.xml"
  val TDN_core_xml = "data/TDN/Corpora/Core/tagset_desc_temp.xml"
  lazy val TDNTagset = TagSet.fromXML(TDN_xml)
  val stylesheet = "src/main/scala/posmapping/tagset-documentation.xsl"

  val coreFeatures = Map(
    "NOU-C" -> List("number", "WF"),
    "NOU-P" -> List("WF"),
    "AA" -> List("degree", "position", "WF"),
    "ADV" -> List("type", "WF"),
    "VRB" -> List("finiteness", "tense", "WF"),
    "NUM" -> List("type", "position", "representation", "WF"),
    "PD" -> List("type", "subtype", "position", "WF"),
    "ADP" -> List("type", "WF"),
    "CONJ" -> List("type", "WF"),
    "INT" -> List("WF"),
    "RES" -> List("type", "WF")
  )

  def integratedTag(tag: String): CHNStyleTag = CHNStyleTag(tag, TDNTagset)
  //case class IntegratedTag(tag: String) extends CHNStyleTag(tag, TDNTagset)

  /*
    Issues:
    - positie bij AA, NUM, PD (dit is de ergste)
    - graad bij AA
    - refl/recip afhankelijk van lemma oplossen
    - w en d pronomina bij vragend/betrekkelijk/aanwijzend afhankelijk van lemma kiezen
   */
  def mapToCore(tag: CHNStyleTag): CHNStyleTag = {
    val mainpos = tag.pos
    val c = coreFeatures(mainpos) ++ List("pos")
    val featsToRemove = tag.features.filter(f => !c.contains(f.name)).map(_.name).toSet
    tag.removeFeatures(featsToRemove)
  }

  def mapToCore(tag: String): String = {
    mapToCore(integratedTag(tag)).toString
  }

  def mapMultipleTagToCore(tag: String): String = tag.split("\\+").map(mapToCore).mkString("+")

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




  def addDescriptionsFromTDN(prefix: String, corpusBased: TagSet, outputName: String = "tagset_desc",
                             corpusName: String, corpusNameInXML: Option[String] = None, fixtags: Boolean = true): TagSet = {

    // val corpusBased = TagSet.fromXML(prefix + fileName) // nee, niet goed


    val corpusBasedWithDesc0 = corpusBased.copy(
      descriptions = TDNTagset.descriptions,
      displayNames = TagSet.mergeDescriptions(TDNTagset.displayNames,
        TDNTagset.descriptions),
      implications = TDNTagset.implications.filter(x =>
        x.corpus.isEmpty || x.corpus.get.contains(corpusName)))

    // println(corpusBasedWithDesc0.valueRestrictions)
    // wat hier nog misgaat is dat door implicatie toegevoegde features niet doorkomen......
    // je moet eigenlijk eerst fixTags doen, en DAN weer opnieuw de hele zaak draaien......

    val zz: TagSet = TagSet.sortFeatures(corpusBasedWithDesc0, TDNTagset)

    val zzz = if (fixtags) zz.fixAllTags() else zz
    val corpusBasedWithDesc = TagSet.sortFeatures(zzz, TDNTagset)

    TagSet.compare(TDNTagset, corpusBasedWithDesc)

    println(corpusName + " " + corpusBasedWithDesc.valueRestrictions.filter(_.featureName == "subtype"))

    val outputBase = prefix + outputName
    printTagsetXML(corpusBasedWithDesc, corpusNameInXML.getOrElse(corpusName), new PrintWriter(s"$outputBase.xml"))

    // Haal in de json versie de niet-bestaande values weg.
    val tagset_existing_values = corpusBasedWithDesc.removeNonExistentValues()
    val jsonWriter = new PrintWriter(s"$outputBase.json")
    jsonWriter.println(tagset_existing_values.asJSON)
    jsonWriter.close()

    val blfWriter = new PrintWriter(s"$outputBase.blf.yaml")
    blfWriter.println(corpusBasedWithDesc.forBlacklab)
    blfWriter.close()

    val blfWriterCHN = new PrintWriter(s"$outputBase.blf.chnStyle.yaml")
    blfWriterCHN.println(corpusBasedWithDesc.forBlacklabCHNStyle)
    blfWriterCHN.close()

    transformToHTML(outputBase, outputBase)
    //compareGysselingToMolex.pretty(molexWithDesc, new PrintWriter("/tmp/molex_tagset_displayNames.xml"))
    corpusBasedWithDesc
  }

  private def transformToHTML(inputBase: String, outputBase: String): Unit = {
    val z = new File(stylesheet)
    if (z.exists) {
      val x = new XSLT(stylesheet)
      x.transform(s"$inputBase.xml", s"$outputBase.html")
      x.transform(inFile = TDN_xml, outFile = TDN_xml.replaceAll("xml", "html"))
    }
  }

  def tagsetFromCorpusFiles(dirName: String, attribute: String, prefix: String = "/tmp/", corpus: String, corpusXML: Option[String] = None,
                            separator: String = "[+]", fixtags: Boolean = true) = { // let op BaB heeft | tussen tags, dat kan misgaan
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

    tagsetFromSetOfTags(prefix, corpus, allDistinctTags, corpusXML, fixtags)
  }

  implicit def setToMap(s: Set[String]): Map[String, String] = s.toIndexedSeq.zipWithIndex.map({case (x,i) => i.toString -> x}).toMap

  def parseTags(s: String) = {
    import scala.util.matching.Regex._
    val s1 = s.replaceAll("\\)([|+])",")<$1>")
    val parts = s1.split("<.>").toList



    val operators = "<.>".r.findAllMatchIn(s1).map(m => m.toString()).toList.map(_.replaceAll("[<>]",""))
    if (parts.size > 1)
      Console.err.println(s"$operators --> $parts")
    (parts, List("") ++ operators)
  }

  def ambiPatch(m: Map[String,String]) = {
    val m1 = m.mapValues(t => t.split("\\)[|+]")).mapValues(r => r.zipWithIndex.map({case (t,i) => if (i == r.size-1) t else t + ")"}))
    m1.flatMap({case (k,a) => a.zipWithIndex.map({case (v,i) => if (i==0) k->v else s"$k[$i]" -> v})})
  }

  def tagsetFromSetOfTags(prefix: String, corpus: String, tagMapping_0: Map[String, String], corpusNameInXML: Option[String] = None, fixtags: Boolean = true) = {

    val tagMapping = ambiPatch(tagMapping_0)

    val wadde: Map[String, (List[String], List[String])] = tagMapping_0.mapValues(parseTags)

    //wadde.foreach(println)

    val allDistinctTags = tagMapping.values.toSet



    val listWriter = new PrintWriter(prefix + "tagList.txt")
    allDistinctTags.toList.sorted.foreach(t => listWriter.println(t))
    listWriter.close()

    val tagset = CHNStyleTags.tagsetFromSetOfStrings("pos", allDistinctTags.flatMap(t => t.split("\\s*\\+\\s*").toSet))

    val xmlWriter = new PrintWriter(prefix + "tagset.xml")

    printTagsetXML(tagset, corpus, xmlWriter)
    xmlWriter.close()


    // Haal in de json versie de niet-bestaande values weg.
    val tagset_existing_values = tagset.removeNonExistentValues()
    val jsonWriter = new PrintWriter(prefix + "tagset.json")
    jsonWriter.println(tagset_existing_values.asJSON)
    jsonWriter.close()

    val blfWriter = new PrintWriter(prefix + "tagset.blf.yaml")
    blfWriter.println(tagset.forBlacklab)
    blfWriter.close()

    val tagsetPlus = addDescriptionsFromTDN(prefix, tagset, "tagset_desc_temp", corpus, corpusNameInXML, fixtags)

    val tmPlus = tagMapping.mapValues(t => {
      val t1 = new CHNStyleTag(t, tagsetPlus)
      val t1Fixed = tagsetPlus.fixTag(t1).toString
      (t,t1Fixed)
    })


    val r = tagMapping.mapValues(s => {
      val t0 = new CHNStyleTag(s, tagsetPlus)
      val t1 = if (fixtags) tagsetPlus.fixTag(t0) else t0
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

    val eindelijk = r.mapValues(_.removeUnforcedFeatures())

    val inElkaarGesleuteld = wadde.map({
      case (key, (tagz, operators)) =>
        val tags = Seq(r(key)) ++ (1 until tagz.size).map(i => {
          val t = r(s"$key[$i]")
          s"${operators(i)}$t"
        })
        //if (tags.size > 1) Console.err.println(tags)
        (key, tagMapping_0(key) -> tags.mkString(""))
      }
    )

    val currentMapping = new PrintWriter(prefix + "tagset.mapped.txt")

    inElkaarGesleuteld.toList.sortBy(_._2._2).foreach({case (tOrg, (t,t1)) => {
      currentMapping.println(s"$tOrg\t$t\t$t1")
    }})
    currentMapping.close()

    val currentMappingHTML =  <html> <table>{inElkaarGesleuteld.toList.sortBy(_._2._2).map({case (tOrg, (t,t1))  => {
      <tr><td>{tOrg}</td><td>{t1}</td></tr>
    }})}</table></html>

    XML.save(prefix + "tagset.mapped.html", currentMappingHTML, "UTF-8")
    (eindelijk, inElkaarGesleuteld)
  }

  def doBaB= {
    val m = tagsetFromCorpusFiles(DataSettings.babDir, "pos", "data/TDN/Corpora/BaB/",
      "BrievenAlsBuit", Some("zeebrieven"), fixtags=false )
    //val m = tagsetFromSetOfTags("data/TDN/Corpora/Gysseling/", "Gysseling", mapping, Some("gysseling_nt"))
    m
  }

  def doCore = {
    val mapping = io.Source.fromFile("data/TDN/tagjes_kern.txt").getLines.map(l => l -> l).toMap
    val m = tagsetFromSetOfTags("data/TDN/Corpora/Core/", corpus="core", mapping)
  }

  def doONW = {
    //tagsetFromCorpusFiles(DataSettings.onwDir, "msd", "data/TDN/Corpora/ONW/", "ONW")
    // tagsetFromCorpusFiles(DataSettings.onwDir, "pos", "/tmp/ONWTags", "ONW")
    val conversionTable = "/tmp/allTags.onwmap.txt"

    val mapping = io.Source.fromFile(conversionTable).getLines().map(_.split("\\t")).map(r => s"${r(0)};${r(1)}" -> r(2)).toMap
    tagsetFromSetOfTags("data/TDN/Corpora/ONW/", "ONW", mapping)
   }

  def doONWFromCorpus = {
    //tagsetFromCorpusFiles(DataSettings.onwDir, "msd", "data/TDN/Corpora/ONW/", "ONW")
    val t = tagsetFromCorpusFiles(DataSettings.onwDir, "pos", "/tmp/ONWTags/", "ONW")
    //addDescriptionsFromTDN("/tmp/ONWTags",t,"ONW", Some("ONW"))
    //val conversionTable = "/tmp/allTags.onwmap.txt"

    //val mapping = io.Source.fromFile(conversionTable).getLines().map(_.split("\\t")).map(r => s"${r(0)};${r(1)}" -> r(2)).toMap
    //tagsetFromSetOfTags("data/TDN/Corpora/ONW/", "ONW", t)
  }

  def doGysselingFromCorpus = {
    val m = tagsetFromCorpusFiles(DataSettings.gysDir, "pos", "data/TDN/Corpora/Gysseling/", "Gysseling", Some("gysseling_nt") )
    //val m = tagsetFromSetOfTags("data/TDN/Corpora/Gysseling/", "Gysseling", mapping, Some("gysseling_nt"))
    m
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

    val (m,gesleuteld) = tagsetFromSetOfTags("data/TDN/Corpora/Gysseling/", "Gysseling", mapping, Some("gysseling_nt"))

    val mappingRows = gesleuteld.toList.sortBy(_._1).map({case (c, t) => <tr><td>{c}</td><td>{t._2.toString}</td><td>{descriptions.getOrElse(c, "")}</td></tr>})
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



    val (m,gesleuteld) = tagsetFromSetOfTags("data/TDN/Corpora/CGN/", "CGN", CGNToTEIWithTDNTags.mapping, Some("CGN_TDN"))
    val m1 = m.mapValues(tag => tag.reduceToCore(coreFeatures))

    val pCore = new PrintWriter("data/TDN/Corpora/CGN/cgn_core.txt")
    m1.toList.sortBy(_._1).foreach({case (cgn,tdn) => pCore.println(s"$cgn\t$tdn")})
    pCore.close()

    val mappingRows = gesleuteld.toList.sortBy(_._2.toString).map({case (c, t) =>
      val p1 = c.replaceAll("\\(.*", "")

      <tr><td>{c}</td><td>{t._2.toString}</td><td>{m1(c)}</td><td>{descriptions.getOrElse(c, "")}</td></tr>})

    val mappingHTML = <table>{mappingRows}</table>

    XML.save("data/TDN/Corpora/CGN/current_mapping.html", mappingHTML, "UTF-8")

    m
    // tagsetFromCorpusFiles("/home/jesse/Links/Werkfolder/Projecten/InterviewsCGN/TEI", "pos", "data/TDN/Corpora/CGN/", "CGN")
  }


   def main(args: Array[String]): Unit = {
     val featureNames = coreFeatures.flatMap(_._2).toSet
       featureNames.foreach(x => {
       val stukkie = s"""
         |      - name: $x
         |        valuePath: "@pos"
         |        uiType: select
         |        multipleValues: true
         |        allowDuplicateValues : false
         |        process:
         |        - action: parsePos
         |          field: $x
         |        - action: split
         |          separator: '[|]'
         |          keep: both
         |""".stripMargin
         println(stukkie)
       })
     // doCGN
     return
      //doONW
      //doGysselingFromCorpus
     // doBaB
     doONWFromCorpus
     val core = false
     if (core) {
       TDNTagset.copy(pos2partitions = coreFeatures).generateTags("core")
       doCore
     }
   }
}

case class MakeTagsetHTMLClass(tagxml: String) {
  import TagsetDiachroonNederlands._
  val stylesheet = "./data/TDN/tdn.xsl"
  private def transformToHTML(): Unit = {
    val z = new File(stylesheet)
    println(s"Transforming $tagxml with $stylesheet")

    if (z.exists) {
      //println(z)
      val x = new XSLT(stylesheet)

      x.transform(inFile = tagxml, outFile = tagxml.replaceAll(".xml", ".latestie.html"))
    }
  }
  def main(args: Array[String]) = {
    transformToHTML()
  }
}

object MakeTagsetHTML extends MakeTagsetHTMLClass(TDN_xml)

object MakeCoreTagsetHTML extends MakeTagsetHTMLClass(TDN_core_xml)

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

