package corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.syntax_poc

import corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.syntax_poc.luchtfietsen.lassy_small
import sext.SextAnyTreeString

import java.io.PrintWriter
import scala.xml._
object TestNodeQueries {
  def labelNode(q: QueryNode, label: String): QueryNode = {
    val children = q.children.zipWithIndex.map({ case (n, i) => labelNode(n, s"${i + 1}") })
    q.copy(children = children, label = label)
  }

  implicit def q(a: TokenQuery) = QueryNode(a)

  implicit def parseTokenQuery(s: String): TokenQuery = {
    val a = s.split("\\s*=\\s*")
    a(0) match {
      case "pos" => PoSQuery(a(1))
      case "rel" => TokenRelQuery(a(1))
      case "lemma" => LemmaQuery(a(1))
      case _ => TokenRelQuery(a(0))
    }
  }

  implicit def s2q(s: String) = QueryNode(parseTokenQuery(s))

  def q(a: TokenQuery, b: QueryNode) = QueryNode(a, Seq(b))
  def q(a: TokenQuery, b: QueryNode, c: QueryNode) = QueryNode(a, Seq(b, c))

  def q(a: TokenQuery, b: QueryNode, c: QueryNode, d: QueryNode) = QueryNode(a, Seq(b, c, d))

  def q(a: TokenQuery, b: QueryNode, c: QueryNode, d: QueryNode, e: QueryNode) = QueryNode(a, Seq(b, c, d, e))

  // [rel='root'
  //    [pos='NOUN' & rel='nsubj'
  //      [pos='ADJ' & lemma='Amerikaans']
  //    ]
  //    [pos='ADV']
  // ]

  val testQueries = List(
    "test0" -> QueryNode.geen_vraagzin,

    "verySimple" -> QueryNode(headProperties = "rel=root",
      children = Seq(
        QueryNode(headProperties = "rel=nsubj"),
        QueryNode(headProperties = "rel=obj"),
      )
    ),

    "twolevel" -> QueryNode(headProperties = "rel=root",
      children = Seq(
        q("rel=nsubj",
          q(PoSQuery("ADJ") & TokenRelQuery("amod"))
        )
      )),
    "moreComplexNesting" ->
      q(
        TokenRelQuery("root"),
        q(
          PoSQuery("NOUN") & TokenRelQuery("nsubj"),
          q(PoSQuery("ADJ") & TokenRelQuery("amod"))
        ),
        q(
          PoSQuery("NOUN") & TokenRelQuery("obj"),
          q(PoSQuery("ADJ") & TokenRelQuery("amod"),
            PoSQuery("ADV") & TokenRelQuery("advmod")
          )
        )
      ),



    "separableVerb" ->
      q(
        TokenRelQuery("root") & LemmaQuery("uit_komen"),
        q(TokenRelQuery("nsubj")),
        q(LemmaQuery("uit") & TokenRelQuery("compound:prt")),
        q(LemmaQuery("er"))
      ),

    "cleft" ->
      QueryNode(headProperties=TokenRelQuery("root"),
         children=Seq(q(TokenRelQuery("ccomp"),
           q(TokenRelQuery("obj"))// & LemmaQuery("wat"))
         )),
       postCondition=CaptureOrderCondition("obj<root")),

    "testNot" ->
      QueryNode(
        headProperties = "rel=root",
        children = Seq("rel=obj"),
        and_not = Seq("rel=nsubj")
      ),

    "threeAdjectives" ->
      q(
        "pos=NOUN" & "nsubj",
        q("pos=ADJ"),
        q("pos=ADJ"),
        q("pos=ADJ")
      ),

    "objectBeforeSubject" ->
      QueryNode(
        headProperties = "rel=root",
        children = Seq(
          "rel=nsubj",
          PoSQuery("NOUN") & TokenRelQuery("obj")
        ),
        and_not = Seq("lemma=?"), // we willen even geen vragen
        //condition = ConditionAnd(Condition.defaultCondition, ChildOrderCondition("0>1")),
        postCondition = CaptureOrderCondition("obj<nsubj") // zo kunnen we nog niets met de volgorde tov van de parent. Toch iets capture-achtigs doen??
      )

      )


  def testQuery(name: String, q0: QueryNode, treebank: Seq[Set[ISpan]] = luchtfietsen.alpino, evaluate: Boolean = true, html:Boolean = false, log:Option[PrintWriter] = None) = {

    val q = labelNode(q0, "0")
    val volgensJan = q.toRelQuery()
    //println(q.treeString.replaceAll("\\|", "\t"))
    if (html && log.nonEmpty) {
      val x = XML.loadString("<pre>" + q.toPseudoCQL(0, true).replaceAll("--([:*a-z]+)-->", "<ruby><font size='5'>⟶</font><rt><i>$1</i></rt></ruby>") + "</pre>")
      log.head.println(<div>
        <h3>Query {name}, pseudoCQL</h3>
        <div>
        {x}
        </div>
        <div>
          <head>Relation query</head>
          <pre>{volgensJan}</pre>
        </div>
      </div>)
    } else {
      println(s"\n\n\n####################################### Query $name #########################################\n" + q.toPseudoCQL(0, html = html))
      println(s"Volgens jan: $volgensJan");
    }
    if (evaluate) {
      println("\n" + volgensJan.treeString.replaceAll("\\|", " "))
      val encoded = java.net.URLEncoder.encode(volgensJan.toString, "UTF-8")

      val searchURL = s"http://svotmc10.ivdnt.loc:8080/blacklab-server/lassy-small/hits?outputformat=xml&wordsaroundhit=20&patt=$encoded"
      val searchResult = XML.load(searchURL)

      println(s"////// $searchURL //////////////////////////////")
      println("////// results: " + RelQueryTests.getNResults(searchResult) + "/////////////////")

      // println(searchResult)childrenToCQL, stukjes_niet
      val hits = (searchResult \\ "hit").map(RelHit(_))
      hits.take(3).foreach(h => {
        println("----------------------------")
        println(h.words.mkString(" "))
        h.relations.foreach(println)
      })
      println("///// compare to naive evaluation ///////")
      luchtfietsen.runQuery(q.nodeQuery(), treebank = treebank, max = 3, printQuery = false)
    }
  }

  def main(args: Array[String]) = {
    val log = new PrintWriter("/tmp/log.html")
    testQueries.foreach({case (n, q) => testQuery(n,q, treebank=lassy_small, log=Some(log)) })
    log.close()
  }
}