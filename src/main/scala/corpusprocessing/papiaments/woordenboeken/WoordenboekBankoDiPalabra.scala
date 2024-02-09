package corpusprocessing.papiaments.woordenboeken
import corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.grouping


import scala.xml._
object WoordenboekBankoDiPalabra {
  val xmlFilename = "/mnt/Projecten/Papiaments/Woordenboeken/WoordenboekBankoDiPalabra/Woordenboek Banko di Palabra.tei.xml"
  lazy val doc = XML.load(xmlFilename)
  lazy val paragraphs = (doc \\ "p")
  import scala.xml.PrettyPrinter
  val p = new PrettyPrinter(200,4)

  def groupRenditions(nodes: Seq[Node])  = {
    def rend(n: Node) = (n \ "@rend").text
    val nn: Seq[(Node, Int)] = nodes.zipWithIndex
    val groups: Seq[Seq[(Node)]] =
      grouping.groupWithFirst[(Node,Int)](nn, { case (n, i) =>  i == 0 || rend(nodes(i-1)) != rend(n) })
        .map(x => x.map(_._1))
    groups.map(g => <hi rend={rend(g.head)}>{g.text}</hi>)
  }

  def postProcessP(p: Element)  = p.copy(child = groupRenditions(p \ "hi"))

  def main(args: Array[String])  = {
    println(paragraphs(10))
    val grouped = postProcessP(paragraphs(100).asInstanceOf[Element])
    println(p.format(grouped))
  }
}
