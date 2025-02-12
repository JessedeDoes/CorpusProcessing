package corpusprocessing.clariah_training_corpora.training_data_extraction.cobaltje

import corpusprocessing.clariah_training_corpora.training_data_extraction.cobaltje.CobaltExport.{checkAllCorpora, downloadFile}
import corpusprocessing.clariah_training_corpora.training_data_extraction.TrainingDataExtraction
import utils.zipUtils

import java.io.File
import java.net.URL
import java.nio.file.{Files, Path}
import scala.sys.process._
import scala.xml._
// http://lexit.inl.loc:8080/CobaltServe/cobalt/export/?project_name=gtbcit_punct_14_refurbished&only_validated=false

import OldCobaltSettings._

object CobaltExport {


  def downloadFile(url: String, filename: String) = {
    new URL(url) #> new File(filename) !!
  }

  def checkAllCorpora(lancelotDB: LancelotDatabase, settings: CobaltExportSettings)  = {
    val corpora: Seq[Map[String, Any]] = lancelotDB.getCorpusInfo().filter(_("schemaName") != "gtbcit_punct_14_refurbished")

    val enhancedInfo: Seq[Map[String, Any]] = corpora.map(corpus => {
      val projectName = corpus("schemaName")

      try {

        Console.err.println(s"Try to export  $projectName")
        val url = s"${settings.cobaltServeExport}?project_name=$projectName&only_validated=false"
        Console.err.println(s"URL for export: $url")
        val e = CobaltExport(url, projectName.toString, settings.downloadDir)
        if (true || e.downloadSuccessfull) {
          val nValid = e.validTokens.size
          val nTokens = e.tokens.size

          if (nTokens == 0 || nValid / nTokens.toDouble < 0.75) {
            System.err.println(s"Corpus $projectName is not ready: $nValid / $nTokens, deleting zip")
            new File(e.zipName).delete()
          } else
            println(
              s"""Downloading $projectName at $url
      Exported to: ${e.zipName}
      Blacklab: docs=${corpus("documentCount")} tokens=${corpus("tokenCount")}
      Export: docs=${e.documents.size} tokens=${e.tokens.size} validated tokens=${nValid}""")
          corpus ++ Map("validatedTokens" -> nValid)
        } else {
          Console.err.println(s"Failed to download cobalt project $projectName")
          corpus
        }

      } catch {
        case e: Exception => e.printStackTrace()
          corpus
      }
    })
    val info = List("schemaName", "documentCount", "tokenCount", "validatedTokens", "url")
    println("INFO:" + info.mkString("\t"))

    enhancedInfo.sortBy(_("schemaName").asInstanceOf[String]).foreach(m => {
      val row = info.map(f => m.getOrElse(f, "-"))
      val include = !m.contains("validatedTokens") || (2 * m("validatedTokens").asInstanceOf[Int] > m("tokenCount").asInstanceOf[Int])
      println(row.mkString("\t"))
    })
  }


  def main(args: Array[String])  = {
    val stats = LancelotDatabase(LancelotSettings)
    checkAllCorpora(stats, LancelotSettings)
  }
}

object OldCobaltExport {
  def main(args: Array[String])  = {
    val stats = LancelotDatabase(OldCobaltSettings)
    checkAllCorpora(stats, OldCobaltSettings)
  }
}

import CobaltExport._
case class CobaltExport(url: String, name: String, downloadDir: String) {

  val zipName = s"$downloadDir/cobalt_export_$name.zip"

  lazy val downloadSuccessfull = HTTPDownload(url, zipName, "jesse", "dedoes").apply()

  lazy val paths: Seq[Path] =
    if (downloadSuccessfull)
      try zipUtils.find(zipName) catch {

        case e: Exception =>
          Console.err.println(s"Exception for $zipName!!! ($name,$url)")
          Seq()
      }
    else {
      val f = new File(zipName)
      if (f.exists()) f.delete()
      Stream[Path]()
    }

  lazy val files = paths.map(p =>  Files.newInputStream(p))
  lazy val documents = files.map(XML.load)
  lazy val tokens = documents.flatMap(d => d \\ "w")
  lazy val validTokens = tokens.filter(w => (w \ "@valid").text == "true")
}



