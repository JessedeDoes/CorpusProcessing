package corpusprocessing.clariah_training_corpora
import utils.PostProcessXML

import scala.xml._

object fixTokenization {

  val bartje = XML.loadFile("/mnt/Projecten/Corpora/Historische_Corpora/TrainingCorpora/Mapped2TDN/NederlabEvaluation15/enge022vand01_01.tok.frogmodernized.sampled.tei.exported.xml")

  def getId(n: Node):String = n.attributes.filter(a => a.prefixedKey.endsWith(":id") || a.key=="id").map(_.value.toString).headOption.getOrElse("zeperd")

  def groupBy(tags: Seq[Node], f: Node=>String, grouper: Seq[Node] => Node) =
     {
       val g0  = tags.zipWithIndex.groupBy({case (n,i) => f(n)}).toList.sortBy(_._2.head._2).map({case (s,l) => (s, l.sortBy(_._2).map(_._1))}) // .mapValues(l => l.sortBy(_._2).map(_._1))
       g0.map({case (s,l) =>  grouper(l)})
     }

   def fix(s: Elem) = {
     def f(n: Node) =  (n \ "@n").headOption.map(_.text).getOrElse(getId(n))
     val groupedChild = groupBy(s.child.filter(_.isInstanceOf[Elem]), f, g => if (g.size==1) g.head else <group>{g}</group>)
     s.copy(child=groupedChild)
   }

  def fixDocje(doc: Elem) = {
    val d0 = PostProcessXML.updateElement(doc, _.label=="s" , fix)

    val d1 = PostProcessXML.updateElement5(d0, _.label=="group", g => {
      if (g.child.size == 2 && g.child.head.text.endsWith("-")) {
        val w0 = g.child.head.asInstanceOf[Elem]
        val w1 = g.child.tail.head.asInstanceOf[Elem]
        // Console.err.println(g)
        w0.copy(attributes = w0.attributes.filter(_.key != "type").append(new UnprefixedAttribute("org", w0.text + w1.text, Null)), child = Text(w0.text.replaceAll("-$", "") + w1.text))
      } else g.child
    })
    d1.asInstanceOf[Elem]
  }

  def main(args: Array[String]): Unit = {
    val x = fixDocje(bartje)
    XML.save("/tmp/bartje.xml", x, "UTF-8")
  }
}
