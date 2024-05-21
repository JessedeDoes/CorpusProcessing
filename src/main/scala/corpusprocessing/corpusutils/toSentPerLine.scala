package corpusprocessing.corpusutils
import corpusprocessing.clariah_training_corpora.patch_scripts.fixTokenization.getId

import scala.xml._
import java.io.File
object toSentPerLine {
  def printSentences(f: String) = {
    val d = XML.load(f)
    val n = new File(f).getName
    val docPid = ((d \\ "interpGrp").filter(x => (x \ "@type").text == "pid") \\ "interp").text
    (d \\ "s").foreach(s => {
      val id = getId(s)
      val t = s.text.replaceAll("\\s+", " ").trim
      println(s"$n\t$docPid\t$id\t$t")
    })
  }


  def main(args: Array[String]) = {

    args.foreach(printSentences)
  }
}
