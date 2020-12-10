package corpusprocessing.bab_aanvulling
import utils.PostProcessXML

import scala.xml._
import util.{Failure, Success, Try}

object fixAbbr {

  def fixW(w: Elem): Elem = {
    val original = (w \ "@original").text
    if (original.nonEmpty && original.contains("<")) {
      val newNode = Try(XML.loadString("<w>" + original + "</w>")) match {
        case Success(x) => w.copy(child = x.child)
        case _ => w
      }
      newNode
    } else w
  }

  val testW = <w original="&lt;abbr&gt;ver&lt;/abbr&gt;staen">verstaen</w>

  def fixDoc(d: Elem) = PostProcessXML.updateElement(d, _.label=="w", fixW)
  def fixAllDocs = new java.io.File(Settings.xmlAfterImageCorrections).listFiles().foreach(
    f => {
      val fixed = fixDoc(XML.loadFile(f))
      val newFile = Settings.abbrCorrectDir + f.getName()
      XML.save(newFile, fixed, "UTF-8")
    }
  )

  def main(args: Array[String]): Unit = {
    // println(fixW(testW))
    fixAllDocs
  }
}
