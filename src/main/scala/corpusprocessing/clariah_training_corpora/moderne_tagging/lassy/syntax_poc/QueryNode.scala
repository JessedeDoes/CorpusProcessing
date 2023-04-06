package corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.syntax_poc



import corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.syntax_poc.QueryNode.q
import corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.syntax_poc.luchtfietsen.{alpino, french_gsd, japanese_bccwj, japanese_combi, japanese_gsd}
import sext._

case class QueryNode(headProperties : TokenQuery,
                     children: Seq[QueryNode] = Seq(),
                     and_not: Seq[QueryNode] = Seq(),
                     condition: ICondition = Condition.defaultCondition,
                     postCondition: ICondition = Condition.trivial,
                     optional: Boolean = false,
                     label:String="unlabeled")
{

  def nodeQuery(): Query = if (children.isEmpty) headProperties else {
    val clauses = children.map(x => x.nodeQuery()).map(x => Queries.headExtend(x))
    val negative_clauses = and_not.map(x => x.nodeQuery()).map(x => Queries.headExtend(x))
    val positive: HeadRestrict = HeadRestrict(Queries.headIntersect(clauses.toSeq, condition=condition, postCondition=postCondition, label=label), headProperties)

    if (negative_clauses.nonEmpty) {
      val negative =  HeadRestrict(Queries.headIntersect(negative_clauses.toSeq), headProperties)
      QueryAndNot(positive, negative)
    } else
      positive
  }

  def toCQL(depth:Int=0) : String = {
    val indent = ("\t" * depth)
    val stukjes = children.map(x => x.toCQL(depth+1)).map(x =>  x).mkString("")

    val stukjes_niet = {
      val p = and_not.map(x => x.toCQL(depth+2)).map(x =>   x)mkString(" ")
      if (p.isEmpty) "" else s"\n$indent\t!($p\n$indent\t)"
    }

    val tp = "(" + headProperties.toCQL(depth+1) + ")"
    val conditionPart = if (condition == Condition.defaultCondition) "" else "\n" + indent + "::" + condition.toString
    val postConditionPart = if (postCondition == Condition.trivial) "" else "\n" + indent + "::" + postCondition.toString
    val parts = List(tp,stukjes,stukjes_niet,conditionPart,postConditionPart).filter(_.nonEmpty).mkString(" ")

    "\n" + (indent) + s"$label:[${parts}]"
  }
}

// [geneste haakjes? [pos="VERB" & lemma="fietsen" [rel="obj"] [rel="nsubj"]]
// vergelijk https://ufal.mff.cuni.cz/pmltqdoc/doc/pmltq_tutorial_web_client.html

/*
Problemen:17      de      de      DET     LID|bep|stan|rest       Definite=Def    18      det     18:det  _
18      Nederlandse     Nederlands      ADJ     ADJ|prenom|basis|met-e|stan     Degree=Pos      15      nmod    15:nmod:van     _
19      Bank    bank    NOUN    N|soort|ev|basis|zijd|stan      Gender=Com|Number=Sing  18      fixed   18:fixed        _

- Volgorde van dependents, ten opzichte van elkaar en ten opzichte van de head
- Meerdere dependents met zelfde rol (bijvoorbeeld drie ADJ bij een znw)
- 'Diepere' relaties (bijvoorbeeld  a ->* b voor b hangt willekeurig diep onder a, kan je constituenten mee maken)
- In plaats van alleen maar captures iets boom-achtigs in het resultaat meegeven?

Vergelijk cypher, https://neo4j.com/developer/cypher/querying/
 */

object QueryNode {
  // Ja dit kan ook compacter met varargs @* enzo maar ik schijn dat niet uit het hoofd te kunnen

  def labelNode(q: QueryNode, label:String):QueryNode = {
     val children = q.children.zipWithIndex.map({case (n,i) => labelNode(n, s"${i+1}")})
     q.copy(children=children, label=label)
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

   def q(a: TokenQuery,b: QueryNode) = QueryNode(a,Seq(b))
   def q(a: TokenQuery,b: QueryNode, c: QueryNode) = QueryNode(a, Seq(b,c))
   def q(a: TokenQuery,b: QueryNode, c: QueryNode, d: QueryNode) = QueryNode(a, Seq(b,c,d))
   def q(a: TokenQuery,b: QueryNode, c: QueryNode, d: QueryNode, e: QueryNode) = QueryNode(a, Seq(b,c,d,e))

  // [rel='root'
  //    [pos='NOUN' & rel='nsubj'
  //      [pos='ADJ' & lemma='Amerikaans']
  //    ]
  //    [pos='ADV']
  // ]

  val testQueries = List(
  "verySimple" -> QueryNode(headProperties = "rel=root",
    children = Seq(
      QueryNode(headProperties = "rel=nsubj"),
      QueryNode(headProperties = "rel=obj"),
    )
  ),

    "moreComplexNesting" ->
      q(
        TokenRelQuery("root"),
        q(
          PoSQuery("NOUN") & TokenRelQuery("nsubj"),
          q(PoSQuery("ADJ"))
        ),
        q(
          PoSQuery("NOUN") & TokenRelQuery("obj"),
          q(PoSQuery("ADJ"),
            PoSQuery("ADV")
          )
        )
      ),

  "threeAdjectives" ->
      q(
        "pos=NOUN" & "nsubj",
        q("pos=ADJ"),
        q("pos=ADJ"),
        q("pos=ADJ")
      ),

  "separableVerb"  ->
    q(
      TokenRelQuery("root") & LemmaQuery("komen"),
      q(TokenRelQuery("nsubj")),
      q(LemmaQuery("uit") & TokenRelQuery("compound:prt")),
      q(LemmaQuery("er"))
    ),




    "testNot" ->
    QueryNode(
      headProperties = "rel=root",
      children = Seq("rel=obj"),
      and_not = Seq("rel=nsubj")
    ),

   "objectBeforeSubject" ->
    QueryNode(
      headProperties = "rel=root",
      children = Seq(
        "rel=nsubj",
        PoSQuery("NOUN") & TokenRelQuery("obj")
      ),
      and_not=Seq("lemma=?"), // we willen even geen vragen
      //condition = ConditionAnd(Condition.defaultCondition, ChildOrderCondition("0>1")),
      postCondition = CaptureOrderCondition("obj<nsubj") // zo kunnen we nog niets met de volgorde tov van de parent. Toch iets capture-achtigs doen??
    ))

   def testQuery(name: String, q0: QueryNode, treebank: Seq[Set[ISpan]] = alpino) = {
     // println(q.treeString.replaceAll("\\|", "\t"))
     val q = labelNode(q0,"0")
     println(s"\n\nQuery $name\n" + q.toCQL())
     luchtfietsen.runQuery(q.nodeQuery(), treebank=treebank, max=3)
   }

   def main(args: Array[String])  = {
     testQueries.foreach({case (n, q) => testQuery(n,q, treebank=alpino) })
   }
}
