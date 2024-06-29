package corpusprocessing.GCND

import utils.XSLT

import java.io.File

object extractAlpinoSentences {

}
object ExtractAlpinoSentencesFromFolia  {
  val stylesheet="data/GCND/XSL/extract_sentences.xsl"
  val folia = "data/GCND/Folia"
  val toDir = "/mnt/Projecten/Hercules/Corpus-ZNL-Dialecten/Alpino/"

  def main(args: Array[String]) : Unit = {
    new File(toDir).listFiles().foreach(_.delete())
    XSLT.main(Array(stylesheet, folia, toDir))
  }
}
