package corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.syntax_poc

import corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.syntax_poc.luchtfietsen.{alpino, basic, lassy_small}
import sext.SextAnyTreeString

import scala.xml.{Node, XML}

object RelQueryTests {
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
    hits.take(50).foreach(h => {
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
  import scala.util.Random
  def testRelQueryDirect(name: String, q: String) = {

    println(s"\n\n############ Query $name=$q ####################")
    val encoded = java.net.URLEncoder.encode(q, "UTF-8")
    val searchURL = s"http://svotmc10.ivdnt.loc:8080/blacklab-server/lassy-small/hits?outputformat=xml&wordsaroundhit=20&patt=$encoded"
    val searchResult = XML.load(searchURL)
    // println(searchResult)
    val hits = (searchResult \\ "hit").map(RelHit(_))
    println("### results: " + getNResults(searchResult))
    Random.shuffle(hits).take(50).foreach(h => {
      // println("------------------")
      println("-" + h.words.mkString(" "))
      h.relations.foreach(r => println("\t"  + r))
    })
  }

  val q0 =
    rel("'dep::nsubj'") ∩
      rspan(rel("'dep::amod'"), "'source'")
  val anySource = rel(spanMode = "'source'")
  val er = rel("'dep::advmod'")   ∩ token("[lemma='er']")

  val z = rel("'dep::nsubj'") ∩ token("[lemma='wie|wat|waarom|hoe']")

  val relQueries: Map[String, RelationOrToken] = Map(
    "z" -> z,
    "dinges" -> rel().__ (rel("_").__(z)),
    "q0" -> q0,
    "q1" -> token("[pos='VERB']") ∩ q0,
    "q2" -> rspan(rel("'dep::nobj'"), "'source'") ∩ q0,
    "er" -> er,
    "qsep" -> // token("[lemma='komen']")
      // ∩
      rspan(rel("'dep::compound:prt'"), "'source'")
        ∩ rspan(er, "'source'")

  )

  val vraagwoorden = List("hoe", "waarom", "wat", "wie").take(1).map(w => s"[lemma='$w']").mkString(" | ")
  val relQueriesDirect = List(
    "q0" -> "rel('dep::obj','target') & [pos='NOUN']",
    "q1" -> "[pos='NOUN'] & rel('dep::obj','target')",
    "q3" -> "[lemma='er' & deprel='advmod'] & rel()",
    "q3a" -> "[lemma='er' & deprel='advmod'] & rel('dep::advmod')",
    "q3b" -> "[lemma='er' & deprel='advmod'] & rel('.*')",
    "q5" -> "[pos='NOUN' & deprel='nsubj'] & rel()",
    "q6" -> "[pos='NOUN' & deprel='nsubj'] & rel('dep::nsubj','target')",
    "q7" -> s"(rel('dep::csubj') | rel('dep::ccomp')) & rspan((rel('_') & $vraagwoorden), 'source')",
    "q8" -> s"rel('dep::ccomp') & ($vraagwoorden)"
  )
  def main(args: Array[String])  : Unit = {

     //relQueries.foreach({case (n, q) => testRelQuery(n,q) })
    relQueriesDirect.foreach({case (n, q) => testRelQueryDirect(n,q) })
  }
}
