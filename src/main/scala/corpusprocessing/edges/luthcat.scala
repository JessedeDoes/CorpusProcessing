package corpusprocessing.edges
import scala.xml._

object luthcat {
    val lutdir = edges.edges_in + "/Lutherse_bijbel"
    val lutjes = new java.io.File(lutdir).listFiles().toStream.map(XML.loadFile)
    val header = lutjes.head \\ "teiHeader"
    lazy val text = lutjes.flatMap(x => x \\ "text").flatMap(_.child)
    lazy val luther = <TEI>{header}<text>{text}</text></TEI>

  def main(args: Array[String]): Unit = {
    XML.save("/tmp/luther.xml", luther, "UTF-8")
  }
}
