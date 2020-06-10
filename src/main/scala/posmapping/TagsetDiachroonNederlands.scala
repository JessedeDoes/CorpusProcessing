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

  def addDescriptionsFromTDN(prefix: String, corpusBased: TagSet, outputName: String = "tagset_desc",
                             corpusName: String): Unit = {

    // val corpusBased = TagSet.fromXML(prefix + fileName) // nee, niet goed



    val corpusBasedWithDesc = corpusBased.copy(
      descriptions = TDNTagset.descriptions,
      displayNames = TagSet.mergeDescriptions(TDNTagset.displayNames,
        TDNTagset.descriptions),
      implications = TDNTagset.implications.filter(x =>
        x.corpus.isEmpty || x.corpus.get.contains(corpusName)))

    TagSet.compare(TDNTagset, corpusBasedWithDesc)

    val outputBase = prefix + outputName
    pretty(corpusBasedWithDesc, corpusName,
      new PrintWriter(s"$outputBase.xml"))

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

  private def tagsetFromSetOfTags(prefix: String, corpus: String, allDistinctTags: Set[String]) = {
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

    addDescriptionsFromTDN(prefix, tagset, "tagset_desc_temp", corpus)
  }

  def doONW = {
      tagsetFromCorpusFiles(DataSettings.onwDir, "msd", "data/TDN/Corpora/ONW/", "ONW")
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
      //doONW
      //doGysseling
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

