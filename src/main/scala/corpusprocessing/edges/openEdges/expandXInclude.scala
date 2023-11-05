package corpusprocessing.edges.openEdges
import scala.xml._
import utils.PostProcessXML
import utils.ProcessFolder
import java.io.File
object expandXInclude {
  def includeBooks(d: Elem, fileName: String) = {
    PostProcessXML.updateElement(d, _.label == "include", x => {
      val href = (x \ "@href").text.replaceAll("content", "ids-fixed")
      val dir = new File(fileName).getParentFile.getCanonicalPath
      val included = XML.load(dir + "/" + href)
      included
    })
  }

  def main(args: Array[String]): Unit = {

    val inDir = Settings.teiDir + "/word-alignments/"
    val outDir = Settings.teiDir + "/include-expanded/"

    new File(outDir).mkdir()

    ProcessFolder.processFolder(new File(inDir), new File(outDir), { case (i, o) =>
      if (i.endsWith(".xml")) {
        val g = new File(i)
        println(o)


        val inDoc = XML.loadFile(g)
        val outDoc = includeBooks(inDoc, g.getCanonicalPath)
        XML.save(o, outDoc, "UTF-8")
      }
    })
  }
}
