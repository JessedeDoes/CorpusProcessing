package corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.lassy_ud_comparison

import corpusprocessing.clariah_training_corpora.moderne_tagging.lassy_ud_comparison.CompareLassyUD
import basex.QueryExample.{getQueryResults, runQuery}
import scala.xml._
import scala.collection.convert.ImplicitConversions.`list asScalaBuffer`
import utils.PostProcessXML.updateElement5
object UseLassyEnhancedWithUD {
  val queries = List(

      """//alpino_ds[.//node[@rel='me']]""",

      """for $n in //node[@rel='me']
                       |let $txt := string-join($n/descendant-or-self::node[@word]/@word,' '),
                       |    $cat := $n/@cat,
                       |    $ud := $n//ud, $deprel := data($ud/dep/@deprel),
                       |    $dud := $n/ud
                       |return <r>{$cat} {$txt} : {$deprel}, {$dud}</r>""".stripMargin)

  def query(q: String) : List[Elem] = {
    getQueryResults("Enhanced", q).map(XML.loadString).toList
  }

  def udHeads(n: Elem): Seq[Node] = {
    def pointsOutside(n: Elem, ud: Node): Boolean = !(n \\ "ud").exists(ud1 => ((ud1 \ "@id").text == (ud \ "@head").text))

    (n \\ "ud").filter(u => pointsOutside(n, u))
  }

  def markUdHead(n: Elem) = {
    val heads = udHeads(n)
    val text = (n \\ "ud").map(x => x \ "@form").map(_.text).mkString(" ")
    val headText = heads.map(x => x \ "@form").map(_.text).mkString(" ")
    n.copy(child = Seq(Text("\n"), <text>{text}</text>, Text("\n"), <head>{headText}</head>, Text("\n")) ++ n.child)
  }

  def main(args:Array[String]): Unit  = {
     val r = query(queries.head).take(2)
     val t1 = r.map(x => updateElement5(x, n => n.label=="node" && (n \ "@cat").nonEmpty, markUdHead))
     println(t1)
  }
}

