package posmapping

import java.io.{File, PrintWriter}

import gysseling.HistoricalTagsetPatching

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

  def pretty(tagset: TagSet, corpusName: String, pw: PrintWriter = new PrintWriter(System.out)): Unit = {
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
      val additions = features.filter(f => !lijstje.contains(f)).zipWithIndex.map({case (x,i) => (x,lijstje.size + i)}).toMap
      val combi = lijstje ++ additions
      (pos, features.sortBy(x => combi(x)))
    }

    t.copy(pos2partitions = t.pos2partitions.map({case (p,f)=> sortPartition(p,f)}))
  }


  def addDescriptionsFromTDN(prefix: String, corpusBased: TagSet, outputName: String = "tagset_desc",
                             corpusName: String): TagSet = {

    // val corpusBased = TagSet.fromXML(prefix + fileName) // nee, niet goed



    val corpusBasedWithDesc0 = corpusBased.copy(
      descriptions = TDNTagset.descriptions,
      displayNames = TagSet.mergeDescriptions(TDNTagset.displayNames,
        TDNTagset.descriptions),
      implications = TDNTagset.implications.filter(x =>
        x.corpus.isEmpty || x.corpus.get.contains(corpusName)))

    val corpusBasedWithDesc = sortFeatures(corpusBasedWithDesc0, TDNTagset)

    TagSet.compare(TDNTagset, corpusBasedWithDesc)

    val outputBase = prefix + outputName
    pretty(corpusBasedWithDesc, corpusName, new PrintWriter(s"$outputBase.xml"))

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

  private def tagsetFromSetOfTags(prefix: String, corpus: String, tagMapping: Map[String, String]) = {

    val allDistinctTags = tagMapping.values.toSet

    val listWriter = new PrintWriter(prefix + "tagList.txt")
    allDistinctTags.toList.sorted.foreach(t => listWriter.println(t))
    listWriter.close()

    val tagset = CHNStyleTags.tagsetFromSetOfStrings("pos", allDistinctTags.flatMap(t => t.split("\\s*\\+\\s*").toSet))

    val xmlWriter = new PrintWriter(prefix + "tagset.xml")

    pretty(tagset, corpus, xmlWriter)
    xmlWriter.close()

    val jsonWriter = new PrintWriter(prefix + "tagset.json")
    jsonWriter.println(tagset.asJSON)
    jsonWriter.close()

    val blfWriter = new PrintWriter(prefix + "tagset.blf.yaml")
    blfWriter.println(tagset.forBlacklab)
    blfWriter.close()

    val tagsetPlus = addDescriptionsFromTDN(prefix, tagset, "tagset_desc_temp", corpus)

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
  }

  def doONW = {
      // tagsetFromCorpusFiles(DataSettings.onwDir, "msd", "data/TDN/Corpora/ONW/", "ONW")
      val conversionTable = "/tmp/allTags.onwmap.txt"

    val mapping = io.Source.fromFile(conversionTable).getLines().map(_.split("\\t")).map(r => s"${r(0)};${r(1)}" -> r(2)).toMap
    tagsetFromSetOfTags("data/TDN/Corpora/ONW/", "ONW", mapping)
   }

  def doGysseling = {
     tagsetFromCorpusFiles(DataSettings.gysDir, "pos", "data/TDN/Corpora/Gysseling/", "gysseling_nt")
  }

  def doCGN = {
    //folia.TDNTest.main(Array())
    tagsetFromSetOfTags("data/TDN/Corpora/CGN/", "CGN", folia.TDNTest.mapping.values.toSet)
    // tagsetFromCorpusFiles("/home/jesse/Links/Werkfolder/Projecten/InterviewsCGN/TEI", "pos", "data/TDN/Corpora/CGN/", "CGN")
  }

   def main(args: Array[String]): Unit = {
      doCGN
      doONW
      doGysseling
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

