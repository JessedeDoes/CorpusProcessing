package corpusprocessing.GCND

import utils.XSLT

import java.io.File
import Settings._

import scala.xml.XML
object extractAlpinoSentences {

}
object ExtractAlpinoSentencesFromFolia  {

  def main(args: Array[String]) : Unit = {
    new File(targetDirectoryForAlpinoSentencesForGretel).listFiles().foreach(_.delete())
    val processor = new XSLT(extractSentencesFromFoliaStylesheet)
    val files = new File(Settings.foliaDir).listFiles().iterator.filter(f => (XML.loadFile(f)  \\ "metadata" \\ "transcriptie_status" \ "label").text =="Alpino (gecorrigeerd)")
    files.foreach(f => {
      Console.err.println(f)
      processor.transform(f.getCanonicalPath, targetDirectoryForAlpinoSentencesForGretel + "/"  + f.getName)
    })
   // XSLT.main(Array(extractSentencesFromFoliaStylesheet, Settings.foliaDir, targetDirectoryForAlpinoSentencesForGretel))
  }
}
