package corpusprocessing.clariah_training_corpora.gysseling

import utils.PostProcessXML

import scala.xml._
import utils.PostProcessXML._
object excludeInterpolations {
  lazy val docje = XML.load("/mnt/Projecten/Corpora/Historische_Corpora/CorpusGysseling/TeIndexeren/2020_07_31/3000.tei.xml")


  def exclude(e: Elem)  = Exclusion(e)()

  def main(args: Array[String]) = {
    // println(String.format("%05d", 100.asInstanceOf[Object]))

    //XML.save("/tmp/docje.xml", d1)
    val d2 = Exclusion(docje)()
    XML.save("/tmp/docje.xml", d2)
  }
}

case class Exclusion(docje: Elem)
{
  lazy val d1 = numberNodes(docje)._1.asInstanceOf[Elem]
  lazy val idIndex = (d1.descendant.filter(_.isInstanceOf[Elem])).map(e => e.attributes.filter(_.key == "id").map(_.value.text).mkString("") -> e).toMap.filter(_._1.nonEmpty)
  def f(n: Int)  = String.format("%05d", n.asInstanceOf[Object])
  def numberNodes(n: Node, k: Int=0): (Node,Int)  = {
    val (newChildren,kk) = n.child.foldLeft[(Seq[Node], Int)](Seq[Node]()->(k+1))({case ((seq, n),node) =>
       val (n1,k1) =  numberNodes(node,n)
       (seq :+ n1) -> k1
    })
    val n1 = n match {
      case e: Elem => e.copy(child=newChildren, attributes = e.attributes.append(new UnprefixedAttribute("position", f(k), Null)))
      case _ => n
    }
    n1 -> (kk)
  }

  case class Span(bibl: Node, from: String, to: String) {
    lazy val properties = (bibl \\ "interpGrp").map(g => {
      val name = (g \ "@type").text
      val value = (g \ "interp").text
      name -> value
    }).filter(_._2.nonEmpty)

    lazy val dateringen = properties.filter(_._1.contains("YearLevel0"))
    lazy val hasDate = dateringen.nonEmpty
    lazy val startstone = idIndex(from)
    lazy val endstone = idIndex(to)

    lazy val later = dateringen.exists(_._2 > "1300")
    override def toString = s"$from-$to: $startPosition-$endPosition, $dateringen:  $wordsIn "

    lazy val startPosition = (startstone \ "@position").text
    lazy val endPosition = (endstone \ "@position").text

    lazy val wordsElementsIn = (d1.descendant.filter(x => Set("pc","w").contains(x.label))).filter(w => {
      val p = (w \ "@position").text
      startPosition < p && endPosition > p
    })
    lazy val wordsIn = wordsElementsIn.map(x => (x \ "seg").text).mkString(" ")
  }

  def isSupplied(w: Elem)  = {
    (w \ "seg").text.trim == (w \ "seg" \ "supplied").text.trim && ((w \ "@type").text == "999")
  }
  def apply()  = {
    val e1 = d1
    val biblz = (e1 \\ "bibl").filter(b => (b \\ "span").nonEmpty)

    val spans = biblz.flatMap(b => {
      (b \\ "span").map(s => Span(b, (s \ "@from").text.replaceAll("#",""), (s \ "@to").text.replaceAll("#","")) )
    })
    spans.filter(_.hasDate).foreach(println)
    val forbidden = spans.filter(_.hasDate).filter(_.later).flatMap(_.wordsElementsIn)
    PostProcessXML.updateElement(d1, x => Set("pc", "w").contains(x.label), w => {
      if (forbidden.contains(w) || isSupplied(w)) w.copy(label = "ex_" + w.label) else w
    })
  }

  val sample = <root><c1/><c2><c3/></c2></root>
}
