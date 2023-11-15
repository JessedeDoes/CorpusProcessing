package corpusprocessing.edges.openEdges

import scala.collection.immutable.Set
import scala.xml._
import utils.ProcessFolder
import utils.PostProcessXML
import corpusprocessing.clariah_training_corpora.patch_scripts.fixTokenization.getId
import java.io.File
object makeWordIdsGloballyUnique {
  def setAttribute(e: Elem, prefix: String, name: String, value: String): Elem = {
    val a = e.attributes.filter(_.key != name).append(new PrefixedAttribute(prefix, name, value, Null))
    e.copy(attributes = a)
  }

  def setAttribute(e: Elem, name: String, value: String): Elem = {
    val a = e.attributes.filter(_.key != name).append(new UnprefixedAttribute(name, value, Null))
    e.copy(attributes = a)
  }

  def makeIdsGloballyUnique(d: Elem): Elem = {
    PostProcessXML.updateElement(d, _.label == "ab", x => PostProcessXML.updateElement(x, e => Set("w", "pc").contains(e.label), y => setAttribute(y, "xml", "id", getId(x) + "." + getId(y))))
  }

  def main(args: Array[String]): Unit = {

    val inDir = Settings.teiDir + "/tokenized/"
    val outDir = Settings.teiDir + "/ids-fixed/"

    new File(outDir).mkdir()

    ProcessFolder.processFolder(new File(inDir), new File(outDir), { case (i, o) =>
      if (i.endsWith(".xml")) {
        val g = new File(i)
        val inDoc = XML.loadFile(g)

        val outDoc = makeIdsGloballyUnique(inDoc)

        XML.save(o, outDoc, "UTF-8")
      }
    })
  }
}
