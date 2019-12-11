package Wp6

import java.io.File

object Settings {
  val XMLDirectory = "data/Missiven"
  val rootAtHome = "/data/CLARIAH/WP6/generalemissiven/generalemissiven/"

  val inputDirectoryatHome = "/data/CLARIAH/WP6/generalemissiven/generalemissiven/TestConversion/"
  val inputDirectoryatWork = "/mnt/Nederlab/Corpusdata/2-PreTEI/generalemissiven/"

  val outputDirectoryatHome = "/data/CLARIAH/WP6/generalemissiven/generalemissiven/Simplified/"
  val outputDirectoryAtWork = "/mnt/Nederlab/Corpusdata/5-TEI/generalemissiven/"

  val inputs = List("/tmp/TestConversion/", inputDirectoryatHome, inputDirectoryatWork)
  val outputs = List(outputDirectoryatHome, outputDirectoryAtWork)

  val inputDirectory = inputs.filter(new File(_).exists()).head
  val outputDirectory = outputs.filter(new File(_).exists()).head


  println(s"$inputDirectory $outputDirectory")

  lazy val allFiles = new File(inputDirectory).listFiles().filter(_.getName.endsWith(".xml")).toStream
}
