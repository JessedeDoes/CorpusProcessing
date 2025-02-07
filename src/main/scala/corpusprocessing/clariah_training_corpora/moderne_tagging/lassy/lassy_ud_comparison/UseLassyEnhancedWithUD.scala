package corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.lassy_ud_comparison

import basex.BaseXQueryIterator
import corpusprocessing.clariah_training_corpora.moderne_tagging.lassy_ud_comparison.CompareLassyUD
import basex.QueryExample.{getQueryResults, runQuery}

import scala.xml._
import scala.collection.convert.ImplicitConversions.`list asScalaBuffer`
import scala.collection.convert.ImplicitConversions.`iterator asScala`
import utils.PostProcessXML.updateElement5
import basex.BaseXQueryIterator

import java.net.URLEncoder
object UseLassyEnhancedWithUD {
  val queries = List(
      """//alpino_ds""",
      """//alpino_ds[.//node[@rel='me']]""",

      """for $n in //node[@rel='me']
                       |let $txt := string-join($n/descendant-or-self::node[@word]/@word,' '),
                       |    $cat := $n/@cat,
                       |    $ud := $n//ud, $deprel := data($ud/dep/@deprel),
                       |    $dud := $n/ud
                       |return <r>{$cat} {$txt} : {$deprel}, {$dud}</r>""".stripMargin)

  var multiHeads = 0
  var nNodes = 0
  var sentence = ""

  def query(q: String) : Iterator[Elem] = {
    BaseXQueryIterator.getQueryResults("Enhanced", q).map(XML.loadString)
  }

  def udHeads(n: Elem): Seq[Node] = {
    def pointsOutside(n: Elem, ud: Node): Boolean = !(n \\ "ud").exists(ud1 => ((ud1 \ "@id").text == (ud \ "@head").text))
    (n \\ "ud").filter(u => pointsOutside(n, u))
  }

  def markUdHead(a: Elem)(parentMap:Map[String,Node])(n: Elem) = {
    nNodes += 1
    val heads = udHeads(n)
    val sentence_id = (a \ "sentence" \ "@sentid").text
    val patt = URLEncoder.encode(s"<s n='.*$sentence_id'/>")
    val link = s"http://svotmc10.ivdnt.loc/corpus-frontend/LassyTei/search/hits?patt=$patt&patternMode=expert"
    val cat = (n \ "@cat").text
    val text = (n \\ "ud").map(x => x \ "@form").map(_.text).mkString(" ")
    val headText = heads.map(x => x \ "@form").map(_.text).mkString(" ")
    val rels = heads.map(x => (x \ "@deprel").text).mkString(" ")

    if (heads.size > 1) {
      Console.err.println(s"$sentence_id: $link, $cat [$text], [$headText], $rels in: $sentence")
      multiHeads = multiHeads+ 1;
    }

    n.copy(child = Seq(Text("\n"), <text>{text}</text>, Text("\n"), <head>{headText}</head>, Text("\n")) ++ n.child)
  }

  def main(args:Array[String]): Unit  = {
     val r = query(queries.head)

     val t1 = r.map(x =>  {
       sentence = (x \\ "sentence").text
       val parentMap: Map[String, Node] = (x \\ "node").flatMap(n =>  {
         val childIds = n.child.filter(_.label == "node").map(x => (x \ "@id").text)
         childIds.map(id => id -> n)}).toMap

       updateElement5(x, n => n.label=="node" && (n \ "@cat").nonEmpty, markUdHead(x)(parentMap))}
     )
     t1.foreach(x => {})
     println(s"Nodes $nNodes, multiHeads $multiHeads")
     // println(t1)
  }
}

