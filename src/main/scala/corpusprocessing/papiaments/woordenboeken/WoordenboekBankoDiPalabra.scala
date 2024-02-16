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
  val posStyle = "font-size:10.0pt;font-style:italic;color:#231f20;white-space:pre-wrap;"
  val collocStyle = "font-size:10.0pt;font-style:italic;color:#231f20;white-space:pre-wrap;"

  val pos = "(bw|bnw|vnw|voegw|lidw|tw|ww|hulpww|koppelww|vz)"
  def isBetNr(n: Node) = rend(n) == betNrStyle && n.text.trim.matches("[0-9]+")
  def isPos(n: Node) = rend(n) == posStyle && (n.text.trim.matches("\\(([a-z]+)\\)") || n.text.trim.matches(s"(.* |^\\()($pos)\\..*"))
  def isGender(n: Node) = rend(n) == posStyle && (n.text.trim.matches("\\((de|het)\\)"))

  def isColloc(n: Node) = rend(n) == collocStyle && n.text.contains("~")
  def isLabel(n: Node)  = rend(n) == posStyle && (n.text.trim.matches("\\((.*)\\)"))


  def splitEntry(n: Node)  = {

  }
  def splitRoman(n: Node) = {
    val t = n.text
    val regex = "(^|.* )(I|II|III|IV|V|VI|VII|VIII|IX)( |$)"
    if (t.matches(regex)) {

      val t1 = t.replaceAll(regex,"$1")
      val t2 = t.replaceAll(regex,"$2")
      // println(s"Mooi $t ==> <$t1> <$t2>")
      Seq(n.asInstanceOf[Elem].copy(child = Text(t1.trim)), <blockMarker>{t2.trim}</blockMarker>)
    } else  {
      if (t.contains("II")) println(t)
      n
    }
  }
  def rend(n: Node) = (n \ "@rend").text

  def hasLemma(n: Node): Boolean =  n.descendant.exists(x => x.label=="form" && (x \ "@type").text == "lemma")

  def groupRenditions(nodes: Seq[Node])  = {

    val nn: Seq[(Node, Int)] = nodes.zipWithIndex
    val groups: Seq[Seq[(Node)]] =
      grouping.groupWithFirst[(Node,Int)](nn, { case (n, i) =>  i == 0 || rend(nodes(i-1)) != rend(n) })
        .map(x => x.map(_._1))
    groups.map(g => <hi rend={rend(g.head)}>{g.text}</hi>)
  }

  def groupSenses(entry: Elem)  = {
    val groups = grouping.groupWithFirst[Node](entry.child.filter(_.isInstanceOf[Elem]), x => x.label=="senseMarker")
    val newChildren = groups.flatMap(g => if (g.exists(_.label=="senseMarker")) Seq(<sense>{g}</sense>) else g)
    entry.copy(child = newChildren)
  }

  def groupBlocks(entry: Elem)  = {
    val groups = grouping.groupWithFirst[Node](entry.child.filter(_.isInstanceOf[Elem]), x => x.label == "blockMarker")
    val newChildren = groups.flatMap(g => if (g.exists(_.label == "blockMarker")) Seq(<sense type="roman">
      {g}
    </sense>) else g)
    entry.copy(child = newChildren)
  }

  def groupParagraphsintoEntries(div: Elem)  = {
    val groups = grouping.groupWithFirst(div.child.filter(_.isInstanceOf[Elem]), hasLemma)
    div.copy(child = groups.map(g => <entry>{g.flatMap(x => x.child)}</entry>))
  }

  def postProcessP(p: Elem)  = {
    val groupedChildren = groupRenditions(p \ "hi").flatMap(x =>
      if (rend(x) == papStyle)
        splitRoman(<form type="papiamentu">{x.child}</form>)
      else if (isBetNr(x))
        <senseMarker>{x.text.trim}</senseMarker>
      else if (isPos(x))
        <pos>{x.child}</pos>
      else if (isGender(x))
        <gram type="gender">{x.child}</gram>
      else if (isLabel(x))
        <label>{x.child}</label>
      else if (isColloc(x))
        <colloc>{x.child}</colloc>
      else x)

    val g1 = if (groupedChildren.size > 0) {
      val head = groupedChildren(0)
      if (rend(head).contains("bold")) {
        <form type="lemma">{head.child}</form> +: groupedChildren.tail
      } else groupedChildren

    }  else groupedChildren
    p.copy(child = g1)
  }

  def main(args: Array[String])  = {
    // println(paragraphs(10))
    val grouped = postProcessP(paragraphs(200).asInstanceOf[Elem])
    // println(p.format(grouped))

    val post0 = updateElement(doc, _.label=="p", postProcessP)
    val post1 = updateElement(post0, _.label=="div", groupParagraphsintoEntries)
    val post2 = updateElement(post1, _.label=="entry", groupBlocks)
    val post3 = updateElement(post2, _.label=="entry", groupSenses)
    val post4 = updateElement(post3, _.label=="sense", groupSenses)
    XML.save("/tmp/dict.xml", post4)
    // println(p.format(post1))
  }
}
