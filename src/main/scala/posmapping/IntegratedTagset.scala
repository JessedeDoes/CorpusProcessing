package posmapping

import java.io.{File, PrintWriter}

import gysseling.HistoricalTagsetPatching

import scala.xml.XML
object DataSettings {
   val onwDir = "/home/jesse/workspace/data-historische-corpora/ONW/ONW-processed-metadata/"
   val gysDir = "../data-historische-corpora/gysseling/gysseling-processed-metadata/"
}

object IntegratedTagset {
  val TDNTagset = TagSet.fromXML("data/TDN/TDN_tagset.xml")

  case class IntegratedTag(tag: String) extends CHNStyleTag(tag, TDNTagset)

  def addDescriptionsFromTDN(prefix: String, fileName: String = "tagset.xml", outputName: String = "tagset_desc"): Unit = {

    val corpusBased = TagSet.fromXML(prefix + fileName)

    TagSet.compare(TDNTagset, corpusBased)

    val corpusBasedWithDesc = corpusBased.copy(
      descriptions = TDNTagset.descriptions,
      displayNames = TagSet.mergeDescriptions(TDNTagset.displayNames,
        TDNTagset.descriptions))

    val outputBase = prefix + outputName
    compareGysselingToMolex.pretty(corpusBasedWithDesc,
      new PrintWriter(s"$outputBase.xml"))

    val jsonWriter = new PrintWriter(s"$outputBase.json")
    jsonWriter.println(corpusBasedWithDesc.asJSON)
    jsonWriter.close()

    val blfWriter = new PrintWriter(s"$outputBase.blf.yaml")
    blfWriter.println(corpusBasedWithDesc.forBlacklab)
    blfWriter.close()
    //compareGysselingToMolex.pretty(molexWithDesc, new PrintWriter("/tmp/molex_tagset_displayNames.xml"))
  }

  def tagsetFromCorpusFiles(dirName: String, attribute: String, prefix: String = "/tmp/",
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

    val listWriter = new PrintWriter(prefix + "tagList.txt")
    allDistinctTags.toList.sorted.foreach(t => listWriter.println(t))
    listWriter.close()

    val tagset = CHNStyleTags.tagsetFromSetOfStrings("pos", allDistinctTags)

    val xmlWriter = new PrintWriter(prefix + "tagset.xml")
    xmlWriter.println(TagSet.pretty.format(tagset.toXML))
    xmlWriter.close()
    val jsonWriter = new PrintWriter(prefix + "tagset.json")
    jsonWriter.println(tagset.asJSON)
    jsonWriter.close()

    val blfWriter = new PrintWriter(prefix + "tagset.blf.yaml")
    blfWriter.println(tagset.forBlacklab)
    blfWriter.close()

    addDescriptionsFromTDN(prefix,"tagset.xml", "tagset_desc_temp")
  }

   def doONW = {
      tagsetFromCorpusFiles(DataSettings.onwDir, "msd", "data/TDN/Corpora/ONW/")
   }

  def doGysseling = {
     tagsetFromCorpusFiles(DataSettings.gysDir, "pos", "data/TDN/Corpora/Gysseling/")
  }

   def main(args: Array[String]): Unit = {
      doONW
      //doGysseling
   }

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
}

