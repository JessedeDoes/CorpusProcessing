package standoffmarkup



import scala.xml._

case class TextUnit(xmi: Node, text: String)
{
  val begin = (xmi \ "@offset").text.toInt
  val end = (xmi \ "@length").text.toInt + begin
  val id = (xmi \ "@id").text
  val typ = xmi.label
  val content = text.substring(begin,end)
  override def toString: String = s"$typ $begin:$end $id"
}

case class Annotation(xmi: Node, text: String) {
  val begin = (xmi \ "@begin").text.toInt
  val end = (xmi \ "@end").text.toInt
  val id = (xmi \ "@id").text
  val typ = xmi.label
  val content = text.substring(begin,end)
  override def toString: String = s"$typ $begin:$end $id"
}

object XMI {

  def getId(n: Node):String = n.attributes.filter(a => a.prefixedKey.endsWith(":id") ||
    a.key.equals("id")).map(a => a.value.toString).headOption.getOrElse("no_id_found")

  val TEIFolder = "/data/CLARIAH/WP6/generalemissiven/6"
  val xmiFolder = "/data/CLARIAH/WP6/generalemissiven/missives-xmi-ids-20200416/6"
  val txtFolder = "/data/CLARIAH/WP6/generalemissiven/split_notes/6"
  val nafFolder = "/home/jesse/workspace/NAF/naf_1.1/6"
  val theIdx = "INT_32cf9a42-176e-33f2-84ba-3abd266079d7"

  val theIds = new java.io.File(TEIFolder).listFiles().map(f => f.getName.replaceAll(".xml",""))


 def getParagraphText(p: Elem) = {
   val p1 = utils.PostProcessXML.updateElement(p, x => x.label=="xfw" || x.label == "xnote", x => <fw/>)
   p1.text
 }

  def testje(theId: String) = {
    val TEIFile = s"$TEIFolder/$theId.xml"
    val xmiFile = s"$xmiFolder/$theId.xmi"
    val txtFile = s"$txtFolder/$theId.txt"
    val nafFile = s"$nafFolder/$theId.naf"

    try {
      val TEI = XML.loadFile(TEIFile)
      val xmi = XML.loadFile(xmiFile)
      val naf = XML.loadFile(nafFile)
      // val txtOld = scala.io.Source.fromFile(txtFile).getLines().map(_.replaceAll(",(\\S)",", $1")).map(_.trim).filter(_.nonEmpty).mkString("\n").trim


      val txt = ((xmi \\ "Sofa") \ "@sofaString").text
      val nafTxt = (naf \\ "raw").text

      val xmiObjects = xmi.child.filter(_.isInstanceOf[Elem]).map(_.asInstanceOf[Elem])
        .filter(x => (x \ "@begin").text.nonEmpty)

      val annotations = xmiObjects.map(Annotation(_, txt))

      val so = standoffmarkup.StandoffMarkup.createStandoffMarkup(TEI)
      val textNode = (TEI \\ "text").head

      val okNodes = Set("p", "head")

      val textFromTEI = (textNode.descendant.filter(x => okNodes.contains(x.label)))
        .map(_.text.replaceAll("\\s+", " ")).mkString(" ")

      val test = textFromTEI.replaceAll("\\s+", "") == txt.replaceAll("\\s+", "")

      // println(textFromTEI.trim.substring(0,100))

      println(s"TEST $theId: $test")

      val paragraphsXMI = annotations.filter(_.typ.contains("aragra")).map(a => a.id -> a).toMap
      val paragraphsNAF = (naf \\ "tunit").map(n => TextUnit(n, nafTxt)).map(a => a.id -> a).toMap
      val paragraphsTEI = textNode.descendant.filter(x => x.label == "p" || x.label == "head").map(p =>
        getId(p) -> p).toMap

      paragraphsTEI.foreach({ case (id, p) =>
        val x = paragraphsNAF.get(id)
        if (x.isDefined) {
          val teiTxt = getParagraphText(p.asInstanceOf[Elem])
          val xmiTxt = x.get.content
          val teiNowhite = teiTxt.replaceAll("\\s+", "")
          val xmiNowhite = xmiTxt.replaceAll("\\s+", "")
          val teiTokens = teiTxt.trim.split("\\s+").toList
          val p1 = StandoffMarkup.createStandoffMarkup(p)

          val teiTokenized =
            StandoffTokenizing.tokenize(p1, _.label == "lb")._1.map(_.word)

          //println(teiTokenized)
          val xmiTokens = xmiTxt.trim.split("\\s+").toList

          val check = (teiNowhite == xmiNowhite) && (teiTokenized == xmiTokens)
          if (!check) {
            println(s"\n#### Mismatch for $id!!! ${teiTxt.length} ${xmiTxt.length}\n$teiTxt\n$xmiTxt\n####")
          }
        } else {
          Console.err.println(s"$id is missing in XMI!!!")
        }
      })

      if (false) annotations.filter(_.typ.contains("oken")).foreach(a => {
        val token = txt.substring(a.begin, a.end)
        val token1 = textFromTEI.substring(a.begin, a.end)
        println(s"|$token/$token1| AT $a")
      })
    } catch {
      case e:Exception => Console.err.println(s"No luck for $theId")
    }
  }

  def main(args: Array[String]): Unit = {
    //println(txtFile)
    theIds.sorted.foreach(testje)
  }
}

/*
<type4:Paragraph xmi:id="5" sofa="1" begin="0" end="23" id="INT_8cda57ba-42bf-35ba-9370-6551f8e9a5d8.TEI.1.text.1.body.1.div.1.head.1"/>
    <type4:Paragraph xmi:id="6" sofa="1" begin="24" end="26" id="INT_8cda57ba-42bf-35ba-9370-6551f8e9a5d8.TEI.1.text.1.body.1.div.1.p.1"/>
    <type4:Paragraph xmi:id="7" sofa="1" begin="27" end="183" id="INT_8cda57ba-42bf-35ba-9370-6551f8e9a5d8.TEI.1.text.1.body.1.div.1.p.2"/>
    <type4:Paragraph xmi:id="8" sofa="1" begin="184" end="211" id="INT_8cda57ba-42bf-35ba-9370-6551f8e9a5d8.TEI.1.text.1.body.1.div.1.p.3"/>
    <type4:Paragraph xmi:id="9" sofa="1" begin="212" end="255" id="INT_8cda57ba-42bf-35ba-9370-6551f8e9a5d8.TEI.1.text.1.body.1.div.1.p.4"/>
    <type4:Paragraph xmi:id="10" sofa="1" begin="256" end="300" id="INT_8cda57ba-42bf-35ba-9370-6551f8e9a5d8.TEI.1.text.1.body.1.div.1.p.5"/>
    <type4:Paragraph xmi:id="11" sofa="1" begin="301" end="356" id="INT_8cda57ba-42bf-35ba-9370-6551f8e9a5d8.TEI.1.text.1.body.1.div.1.p.6"/>
    <type4:Paragraph xmi:id="12" sofa="1" begin="357" end="375" id="INT_8cda57ba-42bf-35ba-9370-6551f8e9a5d8.TEI.1.text.1.body.1.div.1.p.7"/>
    <type4:Paragraph xmi:id="13" sofa="1" begin="376" end="428" id="INT_8cda57ba-42bf-35ba-9370-6551f8e9a5d8.TEI.1.text.1.body.1.div.1.p.8"/>
    <type4:Paragraph xmi:id="14" sofa="1" begin="429" end="615" id="INT_8cda57ba-42bf-35ba-9370-6551f8e9a5d8.TEI.1.text.1.body.1.div.1.p.9"/>

 */