package corpusprocessing.clariah_training_corpora.training_data_extraction.cobaltje

import corpusprocessing.clariah_training_corpora.training_data_extraction.cobaltje.CobaltExport.{downloadFile, testURL}
import utils.zipUtils

import java.io.File
import java.net.URL
import java.nio.file.{Files, Path}
import scala.sys.process._
import scala.xml._
// http://lexit.inl.loc:8080/CobaltServe/cobalt/export/?project_name=gtbcit_punct_14_refurbished&only_validated=false

object CobaltExport {
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
        if (false || projectName.toString.contains("duits")) {
          Console.err.println(s"Trying $projectName")
          val url = s"http://jesse:dedoes@lexit.inl.loc:8080/CobaltServe/cobalt/export/?project_name=$projectName&only_validated=false"
          val e = CobaltExport(url, projectName.toString)
          if (true || e.downloadSuccessfull) {
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

case class CobaltExport(url: String, name: String) {
  val zipName = s"/tmp/cobalt_export_$name.zip"
  lazy val downloadSuccessfull = HTTPDownload(url, zipName, "jesse", "dedoes").apply()
  lazy val paths: Seq[Path] = if (downloadSuccessfull) zipUtils.find(zipName) else Stream[Path]()
  lazy val files = paths.map(p =>  Files.newInputStream(p))
  lazy val documents = files.map(XML.load)
  lazy val tokens = documents.flatMap(d => d \\ "w")
  lazy val validTokens = tokens.filter(w => (w \ "@valid").text == "true")
}
