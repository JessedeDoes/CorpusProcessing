package corpusprocessing.papiaments.woordenboeken
import corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.grouping
import utils.PostProcessXML._


import scala.xml._
object WoordenboekBankoDiPalabra {
  val xmlFilename = "/mnt/Projecten/Papiaments/Woordenboeken/WoordenboekBankoDiPalabra/Woordenboek Banko di Palabra.tei.xml"
  lazy val doc = XML.load(xmlFilename)
  lazy val paragraphs = (doc \\ "p")
  import scala.xml.PrettyPrinter
  val p = new PrettyPrinter(200,4)
  val papStyle = "font-size:10.0pt;color:#231f20;white-space:pre-wrap;"
  val betNrStyle = "font-family:'Times New Roman';font-size:10.0pt;font-weight:bold;color:#231f20;white-space:pre-wrap;"
  def isBetNr(n: Node) = rend(n) == betNrStyle && n.text.trim.matches("[0-9]+")

  def rend(n: Node) = (n \ "@rend").text

  def groupRenditions(nodes: Seq[Node])  = {

    val nn: Seq[(Node, Int)] = nodes.zipWithIndex
    val groups: Seq[Seq[(Node)]] =
      grouping.groupWithFirst[(Node,Int)](nn, { case (n, i) =>  i == 0 || rend(nodes(i-1)) != rend(n) })
        .map(x => x.map(_._1))
    groups.map(g => <hi rend={rend(g.head)}>{g.text}</hi>)
  }

  def postProcessP(p: Elem)  = {
    val groupedChildren = groupRenditions(p \ "hi").map(x =>
      if (rend(x) == papStyle) x.copy(label="papiamentu")
      else if (isBetNr(x)) x.copy(label="senseMarker") else x)

    val g1 = if (groupedChildren.size > 0) {
      val head = groupedChildren(0)
      if (rend(head).contains("bold")) {
        <lemma>{head.child}</lemma> +: groupedChildren.tail
      } else groupedChildren

    }  else groupedChildren
    p.copy(child = g1)
  }

  def main(args: Array[String])  = {
    // println(paragraphs(10))
    val grouped = postProcessP(paragraphs(200).asInstanceOf[Elem])
    // println(p.format(grouped))

    val postProcessed = updateElement(doc, _.label=="p", postProcessP)
    println(p.format(postProcessed))
  }
}
