package corpusprocessing.papiaments.nllb

import java.io.{FileInputStream, FileOutputStream, FileReader, PrintWriter}
import java.util.zip.{GZIPInputStream, GZIPOutputStream}
import scala.xml.Node
object NLLB {

  import org.xml.sax.helpers.DefaultHandler
  import org.xml.sax.{Attributes, InputSource}


  var inTuv = false
  var inTu = false
  var inSeg = false
  var lang = "none"
  class MySAXHandler(handle: Map[String,String] => Unit) extends DefaultHandler {

    val tuMap = collection.mutable.HashMap[String,String]()
    override def startElement(uri: String, localName: String, qName: String, attributes: Attributes): Unit = {

      if (qName == "tuv") {
        lang = attributes.getValue("xml:lang")
        inTuv = true;
      }

      if (qName == "seg") {
        inSeg = true;
      }

      if (qName == "tu") {
        inTu = true;

      }
      //println(s"Start Element: $qName")
    }

    override def characters(ch: Array[Char], start: Int, length: Int): Unit = {
      val data = new String(ch, start, length)
      if (inSeg) {
        tuMap(lang)  = data

      }
    }

    override def endElement(uri: String, localName: String, qName: String): Unit = {
      //println(s"End Element: $qName")

      if (qName == "tuv") {
        inTuv = false;
      }
      if (qName == "tu") {
        inTu = false;
        handle(tuMap.toList.toMap)
        tuMap.clear()
      }

      if (qName == "seg") {
        inSeg = false;
      }
    }
  }

  /*
  <tu>
    <tuv xml:lang="en"><seg>There is no God but Thee, the Omnipotent, the Most Exalted, the Knowing, the Wise.</seg></tuv>
    <tuv xml:lang="pap"><seg>No tin otro Dios sino Bo, esun uniko, esun incomparabel, esun ku sabi tur kos, esun di mas prudente.</seg></tuv>
  </tu>
  <tu>

   */

  val portionSize = 50000;
  var portion: Int = 0;
  var item = 0;

  def w(x: String)  = new PrintWriter(new GZIPOutputStream(new FileOutputStream(x)))
  var writer = w(s"/tmp/portion.$portion.xml.gz")
  def handle(m: Map[String,String])  = {

    item = item + 1

    if (item % portionSize == 0) {
      writer.println("</text></TEI>")
      writer.close()
      portion = portion + 1
      writer =  w(s"/tmp/portion.$portion.xml.gz")
      writer.println("<TEI><text>")
    }

    val x = <tuv><tu>{m("en")}</tu><tu>{m("pap")}</tu></tuv>
    writer.println(x)
  }

  def parse(fileName: String)  = {
    writer.println("<TEI><text>")
    // Create a SAX parser and parse the XML using the custom handler
    val factory = javax.xml.parsers.SAXParserFactory.newInstance()
    val parser = factory.newSAXParser()
    val handler = new MySAXHandler(handle)

    val source = new InputSource(new FileReader(fileName))
    parser.parse(source, handler)
    writer.println("</text></TEI>")
    writer.close()
  }

  val stukje = "/mnt/Projecten/Papiaments/Corpusdata/NLLB/stukje.tmx"
  def main(args: Array[String]): Unit = {
     parse(stukje)
  }
}





/*
Use fastAlign to add word alignment to verse-aligned bible XML
 */







