package Wp6

import java.io.PrintWriter

import scala.xml._
import scala.util.matching.Regex._
import StandoffTokenizing._
import Wp6.IndexMatching.part6Files
import utils.PostProcessXML.pushOptionInside

trait Markup {
  def start: Int
  def end: Int
  def name: String
  def value: Any
}

case class StandoffMarkup(start: Int, end: Int, name: String, value: Any) {

}

case class NodeWithOffsets(node: Node, start: Int, end: Int, children: Seq[NodeWithOffsets]) extends Markup
{
  val name = "label"
  lazy val label = node.label
  lazy val value = label
  lazy val length = end - start
  def \(s: String): Seq[NodeWithOffsets] =
    if (s.startsWith("@")) {
      val a = node \ s
      a.map(x => NodeWithOffsets(x, start, start, Seq()))
    } else children.filter(_.node.label == s)
  def \\(s: String): Seq[NodeWithOffsets] = \(s) ++ children.flatMap(x => x.\\(s))
  lazy val text = node.text
  lazy val id = node.attributes.find(_.key == "id").map(_.value.text).getOrElse("id_unknown")
  lazy val descendant : Seq[NodeWithOffsets] = children.flatMap(c => Seq(c) ++ c.descendant)
}

object TEI2NAF {

  def toNaf(n: NodeWithOffsets): Node = {
    if (n.node.isInstanceOf[Text])
      <text offset={n.start.toString} length={n.length.toString}/>
    else {
      val down = n.children.map(toNaf)
      val attributes = n.node.attributes.map(a => <attribute name={a.prefixedKey} value={a.value.text}/>)
      <element id={n.id} offset={n.start.toString} length={n.length.toString} name={n.label}>
        {attributes}{down}
      </element>
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
        <tei>{toNaf(n1)}</tei></NAF>
     })
   }

  val exampleFile = "data/CRM/Metadata/0001.tei.xml"
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

object StandoffMarkup {

  def pushOptionInside(o: Option[(NodeWithOffsets,Int)]):(Option[NodeWithOffsets], Int) =
    o.map(x => (Some(x._1).asInstanceOf[Option[NodeWithOffsets]],x._2)).getOrElse( (None, 0) )

  def extractPages(d: NodeWithOffsets) = {

    //println(d \\ "pb")

    val leafSpul = d.descendant.filter(_.children.isEmpty)

    //println(leafSpul.map(_.label).toSet)

    val pages: Seq[(Option[Int], (Int, Int))] = groupWithFirst(leafSpul, x => x.label == "pb").map(g =>
      (if (g.head.label == "pb") Some((g.head \ "@n").text.toInt) else None) -> (g.head.start -> g.last.end)
    )
    pages.filter(_._1.nonEmpty).map{case (Some(p), (s,e)) => StandoffMarkup(s,e,"page", p)}
  }

  def groupWithFirst(l: Seq[NodeWithOffsets], f: Node => Boolean): Seq[Seq[NodeWithOffsets]] =
  {
    val numberedChild:List[(NodeWithOffsets, Int)] = l.toList.zipWithIndex
    def lastBefore(i:Int):(Option[NodeWithOffsets],Int) = pushOptionInside(numberedChild.filter({case (n,j) => j <= i && f(n.node)}).lastOption)
    val grouped = numberedChild.groupBy({case (n,i) => lastBefore(i)})
    grouped.keySet.toList.sortBy(_._2).map(grouped).map(l => l.map(_._1)) // ahem, unorded...
  }

  implicit class NodeWithOffsetsSeq(s: Seq[NodeWithOffsets]) {
    def text = s.map(_.text).reduce(_ + _)
    def \(w: String) = s.flatMap(x => x \ w)
    def \\(w: String) = s.flatMap(x => x \\ w)
  }

  def createStandoffMarkup(e: Node, startPosition: Int=0): NodeWithOffsets = {
    val initial = (Seq[NodeWithOffsets](), startPosition)

    def addNode(seq: Seq[NodeWithOffsets], p: Int, n: Node) = {
      val n1 = createStandoffMarkup(n, p)
      (seq ++ Seq(n1), n1.end)
    }

    val (children, z): (Seq[NodeWithOffsets], Int) = e.child.foldLeft(initial) { case ((seq, p), n) => addNode(seq, p, n) }
    val last = if (children.nonEmpty) children.last.end else startPosition + e.text.length

    NodeWithOffsets(e, startPosition, last, children)
  }

  def pageTest = {
    part6Files.foreach(f => {
      Console.err.println(f.getCanonicalPath)
      val d = XML.loadFile(f)
      println(extractPages(createStandoffMarkup(d)))
    })
  }

  val dinges = "/tmp/match.INT_d9e3fee8-0b2e-36d9-910b-9681e4d60ef4.xml"
  val dattes = "data/CRM/Metadata/Meertens-CRM-1-1.0ab6990f-9605-3a1d-8fc2-74eb0ec445a0.xml"

  def main(args: Array[String]): Unit = {
    val x1 = createStandoffMarkup(XML.loadFile(dinges))


    val inlineTags = Set("lb", "br", "milestone", "hi", "pb", "expan")

    val (s,b) = tokenize(x1, n => !(inlineTags.contains(n.label)))

    //val words = (x1 \\ "w").text
    //println(words)
    val text = x1.text
    s.foreach(t => {
      val check = text.substring(t.start, t.end)
      println(s"$t <$check>")
    })

    val names: Seq[NodeWithOffsets] = x1 \\ "name"
    if (false) names.foreach(n => {
      val check = n.node.text
      val content = text.substring(n.start, n.end)
      Console.err.println(s"$content####$check###${n.start}#${n.end}")
    })
    //println(x)
  }
}
