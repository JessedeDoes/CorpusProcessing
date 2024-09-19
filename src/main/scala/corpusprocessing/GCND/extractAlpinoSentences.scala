package corpusprocessing.GCND

import utils.XSLT

import java.io.File
import Settings._
object extractAlpinoSentences {

}
object ExtractAlpinoSentencesFromFolia  {

  def main(args: Array[String]) : Unit = {
    new File(targetDirectoryForAlpinoSentencesForGretel).listFiles().foreach(_.delete())
    XSLT.main(Array(extractSentencesFromFoliaStylesheet, Settings.foliaDir, targetDirectoryForAlpinoSentencesForGretel))
  }
}
