package standoffmarkup

import java.io.PrintWriter

import scala.xml.{Elem, Node, Text, XML}

object TEI2NAF {

  def toNaf(n: NodeWithOffsets): Node = {
    val nodeType = n.node.getClass.getName.replaceAll(".*\\.","")
    if (n.node.isInstanceOf[Text])
        <textnode offset={n.start.toString} length={n.length.toString}/>
    else  if (n.node.isInstanceOf[Elem]) {
      val down = n.children.map(toNaf)
      val attributes = n.node.attributes.map(a => <attribute name={a.prefixedKey} value={a.value.text}/>)
      <element id={n.id} offset={n.start.toString} length={n.length.toString} name={n.label}>{attributes}{down}</element>
    } else {
        <node type={nodeType} offset={n.start.toString} length={n.length.toString}/>
    }
  }

  def toTextUnits(n: NodeWithOffsets): scala.xml.NodeSeq = {
    val nodeType = n.node.getClass.getName.replaceAll(".*\\.","")
    if (n.node.isInstanceOf[Text])
        Seq() // <textnode offset={n.start.toString} length={n.length.toString}/>
    else  if (n.node.isInstanceOf[Elem]) {
      val down = n.children.flatMap(toTextUnits)
      val attributes = n.node.attributes.map(a => <attribute name={a.prefixedKey} value={a.value.text}/>)
      <tunit id={n.id} offset={n.start.toString} length={n.length.toString} class={n.label}/> ++ down
    } else {
       Seq() // <node type={nodeType} offset={n.start.toString} length={n.length.toString}/>
    }
  }

  def tei2naf(d: Elem) = {
    val textNode = (d \\ "text").headOption
    textNode.map(n => {
      val n1 = StandoffMarkup.createStandoffMarkup(n,0)
      val txt = n1.text
      // "<![CDATA["
      lazy val pcdata = txt // scala.xml.Unparsed("" + txt) //  "]]>")
      lazy val cdata =  scala.xml.Unparsed("<![CDATA[" + txt   + "]]>")
      val descendants = n1.descendant
      <NAF><raw>{cdata}</raw>
        <textStructure type="TEI" namespace="http://www.tei-c.org/ns/1.0">{toTextUnits(n1)}</textStructure></NAF>
    })
  }

  val missivenTEIDir = "/data/CLARIAH/WP6/generalemissiven/6/"
  val exampleFile = missivenTEIDir + "INT_fa840fc6-9b64-3966-b53d-8a4c79bbec9b.xml"// "data/CRM/Metadata/0001.tei.xml"
  lazy val exampleTEI0 = XML.loadFile(exampleFile)

  val mini = <TEI><text><w>Hallo</w> <w>meneer</w></text></TEI>

  def main(args: Array[String]): Unit = {


    val exampleTEI = if (args.size > 0) XML.loadFile(args(0)) else exampleTEI0
    val w_tei = (exampleTEI \\ "w").map(_.text)
    val nafje0 = tei2naf(exampleTEI).get

    XML.save("/tmp/nafje.xml", nafje0, "UTF-8")

    val nafje = XML.loadFile("/tmp/nafje.xml")

    val naf_txt: String = (nafje \\ "raw").text

    val pw = new PrintWriter("/tmp/txt.txt")
    pw.print(naf_txt)
    pw.close()
    val w_naf = (nafje \\ "element").filter(e => (e \ "@name").text == "w").map(w => {
      val offset = (w \ "@offset").text.toInt
      val length = (w \ "@length").text.toInt
      val id = (w \ "@id").text
      s"$id($offset,$length)=" + naf_txt.substring(offset, offset + length).trim
    })
    //w1.foreach(println)

    w_naf.take(50).zip(w_tei.take(50)).foreach(println)

    //println(nafje)
  }
}