package corpusprocessing.clariah_training_corpora.training_data_extraction.cobaltje

import corpusprocessing.clariah_training_corpora.training_data_extraction.cobaltje.CobaltExport.{downloadFile, testURL}
import corpusprocessing.clariah_training_corpora.training_data_extraction.extract_training_data_trait
import utils.zipUtils

import java.io.File
import java.net.URL
import java.nio.file.{Files, Path}
import scala.sys.process._
import scala.xml._
// http://lexit.inl.loc:8080/CobaltServe/cobalt/export/?project_name=gtbcit_punct_14_refurbished&only_validated=false

object CobaltExport {
  val outputBase = "/mnt/Projecten/Corpora/TrainingDataForTools/CobaltExport/2024/"
  val project = "courantenselectie"
  val testURL = s"http://jesse:dedoes@lexit.inl.loc:8080/CobaltServe/cobalt/export/?project_name=$project&only_validated=false"

  def downloadFile(url: String, filename: String) = {
    new URL(url) #> new File(filename) !!
  }

  def checkAllCorpora()  = {
    val corpora = cobaltStats.getCorpusInfo()
    corpora.foreach(corpus => {
      val projectName = corpus("schemaName")

      try {
        if (true || projectName.toString.contains("duits")) {
          Console.err.println(s"Trying $projectName")
          val url = s"http://jesse:dedoes@lexit.inl.loc:8080/CobaltServe/cobalt/export/?project_name=$projectName&only_validated=false"
          val e = CobaltExport(url, projectName.toString)
          if (true || e.downloadSuccessfull) {
            val nValid = e.validTokens.size
            val nTokens = e.tokens.size
            if (nTokens == 0 || nValid / nTokens.toDouble< 0.75)
              {
                System.err.println(s"Corpus $projectName is not ready: $nValid / $nTokens, deleting zip")
                new File(e.zipName).delete()
              } else
            println(
              s"""Downloading $projectName at $url
      Exported to: ${e.zipName}
      Blacklab: docs=${corpus("documentCount")} tokens=${corpus("tokenCount")}
      Export: docs=${e.documents.size} tokens=${e.tokens.size} validated tokens=${e.validTokens.size}""")
          } else {
            Console.err.println(s"Failed to download cobalt project $projectName")
          }
        }
      } catch {
        case e: Exception => e.printStackTrace()
      }
    })
  }

  def testje() = {
    val export = new CobaltExport(testURL, "aapje")
    export.tokens.foreach(println)
    export.paths.foreach(p => {
      val c = p.getNameCount
      (0 to c - 1).foreach(i => println(p.getName(i)))
    })
  }
  def main(args: Array[String])  = {
    checkAllCorpora()
  }
}

import CobaltExport._
case class CobaltExport(url: String, name: String) {
  val zipName = s"$outputBase/cobalt_export_$name.zip"
  lazy val downloadSuccessfull = HTTPDownload(url, zipName, "jesse", "dedoes").apply()
  lazy val paths: Seq[Path] = if (downloadSuccessfull) zipUtils.find(zipName) else {
    val f = new File(zipName)
    if (f.exists()) f.delete()
    Stream[Path]()
  }
  lazy val files = paths.map(p =>  Files.newInputStream(p))
  lazy val documents = files.map(XML.load)
  lazy val tokens = documents.flatMap(d => d \\ "w")
  lazy val validTokens = tokens.filter(w => (w \ "@valid").text == "true")
}

case class ExtractorFromZippie(zipFileName: String, outputPrefix: String, sentenceElement: String="s") {
  lazy val paths: Seq[Path] = zipUtils.find(zipFileName)



  case class Extractor() extends extract_training_data_trait {
    override val split_test_train_on_document_level = !outputPrefix.contains("evaluation_set")
    override lazy val output_prefix: String = outputPrefix
    override val sentence_element: String = "s"
    override val enhance: Boolean = true
    // override lazy val output_folder = outputDir
  }

  def extract() = {
    val e = Extractor()
    println(e.output_folder)
    e.makeTrainingMaterialAndPartitionFromPaths(paths,e.output_prefix)
  }
}

object ExtractorFromZippie {
  def main(args: Array[String])  = {
    val dir = "/mnt/Projecten/Corpora/TrainingDataForTools/CobaltExport/2024/"
    new File(dir).listFiles().filter(_.getName.endsWith(".zip")).foreach(f => {
      val outputFilenamePrefix = f.getName().replaceAll("^cobalt_export_","").replaceAll(".zip", "")
      new File(dir + "/"  + outputFilenamePrefix).mkdir()
      val outputPrefix = dir + "/" + outputFilenamePrefix + "/"  + outputFilenamePrefix
      val e = ExtractorFromZippie(f.getCanonicalPath, outputPrefix, sentenceElement = if (outputFilenamePrefix.contains("cit")) "q" else "s")
      e.extract()
    })
  }
}
