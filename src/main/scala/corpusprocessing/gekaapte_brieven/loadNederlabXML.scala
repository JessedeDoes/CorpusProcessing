package corpusprocessing.gekaapte_brieven

import java.io.File
import scala.xml.{Comment, Elem, XML}

object loadNederlabXML {

  import Settings._

  val justChecking = true

  lazy val nederBrieven: Iterator[Elem] = new File(nederlabXML).listFiles().filter(_.getName.endsWith(".xml")).iterator.map(XML.loadFile)

  lazy val alleDivs  =
    nederBrieven
      .flatMap(x => (x \\ "div1").map(d => x -> d))
      .map({case (doc, d) =>
        val comments = d.descendant.filter(_.isInstanceOf[Comment])
        val metadata = doc \\ "listBibl"
        val strippedComments = comments.map(x => x.toString.trim.replaceAll("<!--", "").replaceAll("-->", "").replaceAll(",.*$", "").trim) //  Brief id: 1409, Sourcetabel: table-1409.xml
        // Console.err.println(strippedComments)
        val possibleId = strippedComments.filter(x => x.matches("Brief id:\\s*[0-9]+")).map(x => x.replaceAll("Brief id:\\s*", "")).headOption
        if (possibleId.isEmpty || !possibleId.map(_.matches("[0-9]+")).get) {
           Console.err.println("Id not found:" + (possibleId, d.toString()))}
        (possibleId, d.toString(), metadata.toString())
      })
      val nederDivs = alleDivs.filter(_._1.nonEmpty)
      .map({ case (id, div, meta) => (id.get.toInt, div, meta) }) // .take(100)

  /*
  <!--Brief id: 23-->
   */

  def pieterNaarDB(): Unit = {
    preparation.foreach(briefdb.runStatement(_))
    val f: ((Int, String, String)) => Seq[briefdb.Binding] = {
      case (id, xml, metadata) => Seq(briefdb.Binding("id", id), briefdb.Binding("xml", xml), briefdb.Binding("metadata", metadata))
    }
    val b = briefdb.QueryBatch[(Int, String, String)]("insert into nederlab_xml (id,xml, metadata) values (:id, :xml, :metadata)", f)
    b.insert(nederDivs.toStream)
  }

  def main(args: Array[String]): Unit = {
    if (justChecking)
      nederDivs.foreach(println)
    else
      pieterNaarDB()
  }
}
