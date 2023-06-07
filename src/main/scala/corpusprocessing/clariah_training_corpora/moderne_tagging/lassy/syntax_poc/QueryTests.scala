package corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.syntax_poc

import corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.syntax_poc.luchtfietsen.{alpino, lassy_small}
import sext.SextAnyTreeString

import scala.xml.{Node, XML}

object QueryTests {
  // Ja dit kan ook compacter met varargs @* enzo maar ik schijn dat niet uit het hoofd te kunnen


  def testRelQuery(name: String, q0: RelationOrToken, treebank: Seq[Set[ISpan]] = alpino) = {


    val volgensJan = q0
    println(s"\nCQL-rel: $name=$volgensJan\n" + volgensJan.treeString.replaceAll("\\|", " "))
    val encoded = java.net.URLEncoder.encode(volgensJan.toString, "UTF-8")
    val searchURL = s"http://svotmc10.ivdnt.loc:8080/blacklab-server/lassy-small/hits?outputformat=xml&wordsaroundhit=20&patt=$encoded"
    val searchResult = XML.load(searchURL)

    println("### results: " + getNResults(searchResult))
    // println(searchResult)
    val hits = (searchResult \\ "hit").map(RelHit(_))
    hits.take(3).foreach(h => {
      println("----------------------------")
      println(h.words.mkString(" "))
      h.relations.foreach(println)
    })
    // luchtfietsen.runQuery(q.nodeQuery(), treebank=treebank, max=3,printQuery = false)
  }
  // rel('dep::obj','target') & [pos='NOUN']

  def getNResults(n: Node)  = {
    <numberOfHits>1237</numberOfHits>
      <numberOfHitsRetrieved>1237</numberOfHitsRetrieved>
    val numberOfHits = (n \\ "numberOfHits").text
    val numberOfHitsRetrieved  = (n \\ "numberOfHitsRetrieved").text
    numberOfHits -> numberOfHitsRetrieved
  }
  def testRelQueryDirect(name: String, q: String) = {

    println(s"\n##### : $name=$q\n")
    val encoded = java.net.URLEncoder.encode(q, "UTF-8")
    val searchURL = s"http://svotmc10.ivdnt.loc:8080/blacklab-server/lassy-small/hits?outputformat=xml&wordsaroundhit=20&patt=$encoded"
    val searchResult = XML.load(searchURL)
    // println(searchResult)
    val hits = (searchResult \\ "hit").map(RelHit(_))
    println("### results: " + getNResults(searchResult))
    hits.take(3).foreach(h => {
      println("------------------")
      println(h.words.mkString(" "))
      h.relations.foreach(println)
    })
  }

  val q0 =
    rel("'dep::nsubj'") ∩
      rspan(rel("'dep::amod'"), "'source'")
  val anySource = rel(spanMode = "'source'")
  val er = rel("'dep::advmod'")   ∩ token("[lemma='er']")

  val relQueries: Map[String, RelationOrToken] = Map(
    "q0" -> q0,
    "q1" -> token("[pos='VERB']") ∩ q0,
    "q2" -> rspan(rel("'dep::nobj'"), "'source'") ∩ q0,
    "er" -> er,
    "qsep" -> // token("[lemma='komen']")
      // ∩
      rspan(rel("'dep::compound:prt'"), "'source'")
        ∩ rspan(er, "'source'")
  )

  val relQueriesDirect = List(
    "q0" -> "rel('dep::obj','target') & [pos='NOUN']",
    "q1" -> "[pos='NOUN'] & rel('dep::obj','target')",
    "q3" -> "[lemma='er' & deprel='advmod'] & rel()",
    "q3a" -> "[lemma='er' & deprel='advmod'] & rel('dep::advmod')",
    "q3b" -> "[lemma='er' & deprel='advmod'] & rel('.*')",
    "q5" -> "[pos='NOUN' & deprel='nsubj'] & rel()",
    "q6" -> "[pos='NOUN' & deprel='nsubj'] & rel('dep::nsubj','target')",
  )
  def main(args: Array[String])  = {

    relQueries.foreach({case (n, q) => testRelQuery(n,q) })
    relQueriesDirect.foreach({case (n, q) => testRelQueryDirect(n,q) })
  }
}
