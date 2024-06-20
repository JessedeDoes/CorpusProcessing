package utils

import org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl
object HTML {

  import org.xml.sax.InputSource

  import java.io.ByteArrayInputStream
  import scala.xml.Node
  import scala.xml.parsing.NoBindingFactoryAdapter




  lazy val adapter = new NoBindingFactoryAdapter()
  lazy val parser = (new SAXFactoryImpl).newSAXParser

  def parse(f: java.io.File): Node = {
    val lines = io.Source.fromFile(f).getLines()
    // Console.err.println(lines.toList)
    val content = lines.mkString("\n")
    val contentX = HTML.parse(content)
    contentX
  }

  def parse(html: String, encoding: String = "UTF-8"): Node = {
    return this.parse(html.getBytes(encoding))
  }

  def parse(html: Array[Byte]): Node = {

    val stream = new ByteArrayInputStream(html)
    val source = new InputSource(stream)
    return adapter.loadXML(source, parser)

  }

}