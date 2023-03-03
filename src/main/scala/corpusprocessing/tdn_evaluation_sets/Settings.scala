package corpusprocessing.tdn_evaluation_sets

import scala.xml._
import java.io.File
import scala.util.{Try,Success,Failure}
object Settings {
  val inDir = "/mnt/Projecten/Nederlab/Tagging/Cobalt/ExportAll"
  val outDir = "/mnt/Projecten/Nederlab/Tagging/Cobalt/Mapped2TDN"
  val mappingFile = "data/TDN/Corpora/NederlabEval/nederval2tdncore.txt"
  val coreTags = io.Source.fromFile("data/TDN/tagjes_kern.txt").getLines().toSet
  val mapping  = io.Source.fromFile(mappingFile).getLines.map(_.split("\\t")).map(a => a(0) -> a(1)).toMap

  def slurp(f: String) = io.Source.fromFile(f).getLines().mkString("\n")

  def main(args: Array[String]): Unit = {
    utils.ProcessFolder.processFolder(new File(inDir), new File(outDir), {
      case (f1,f2) => {
      if (f1.endsWith(".xml"))  Try(XML.loadString(slurp(f1)
        .replaceAll("&([cç ])", "&amp;$1")
        .replaceAll("lemma=\"<", "lemma=\"&lt;"))) match {
        case Success(d1) => d1
          println(s"$f1 parsed to ${d1.label}")
          val d2 = nederlabEvalToTDN.map(d1)
          XML.save(f2, d2, "UTF-8")
        case Failure(e) => Console.err.println(s"No luck with $f1: $e"); <bah/>
      }
    }})
  }
}
