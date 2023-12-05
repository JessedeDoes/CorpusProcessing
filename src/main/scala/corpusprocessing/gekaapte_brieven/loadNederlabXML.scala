package corpusprocessing.gekaapte_brieven

import java.io.File
import scala.xml.{Comment, Elem, XML}

object loadNederlabXML {

  import Settings._

  val justChecking = false

  lazy val nederBrieven: Iterator[Elem] = new File(nederlabXML).listFiles().filter(_.getName.endsWith(".xml")).iterator.map(XML.loadFile)
  lazy val extraXMLsFromExcel= new File(Settings.extraXMLFromExcel).listFiles().filter(_.getName.endsWith(".xml")).iterator.map(f => f.getName -> XML.loadFile(f)).toMap

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

  def loadExtraExcels() = {
    val missingText: Seq[Map[String, String]] = briefdb.slurp(briefdb.allRecords("brieven_zonder_inhoud")).filter(x => x("xml_to_use") != null && x("xml_to_use").contains("xml"))
    briefdb.runStatement("create table nederlab_excel_xml_more (id integer, xml text)")
    val loadMe: Seq[(Int, String)] = missingText.map(m => m("brief_id").toInt -> extraXMLsFromExcel(m("xml_to_use")).toString() )
    val f: ((Int, String)) => Seq[briefdb.Binding] = {
      case (id, xml) => Seq(briefdb.Binding("id", id), briefdb.Binding("xml", xml))
    }
    val b = briefdb.QueryBatch[(Int, String)]("insert into nederlab_excel_xml_more (id,xml) values (:id, :xml)", f)
    b.insert(loadMe.toStream)
  }

  def pieterNaarDB(): Unit = {
    preparation_for_nederlab_import.foreach(briefdb.runStatement(_))
    val f: ((Int, String, String)) => Seq[briefdb.Binding] = {
      case (id, xml, metadata) => Seq(briefdb.Binding("id", id), briefdb.Binding("xml", xml), briefdb.Binding("metadata", metadata))
    }
    val b = briefdb.QueryBatch[(Int, String, String)]("insert into nederlab_xml (id,xml, metadata) values (:id, :xml, :metadata)", f)
    b.insert(nederDivs.toStream)
  }

  def main(args: Array[String]): Unit = {
    if (justChecking)
      nederDivs.foreach(println)
    else {
      loadExtraExcels()
      // pieterNaarDB()
    }
  }
}
