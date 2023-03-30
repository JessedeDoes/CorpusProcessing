package corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.syntax_poc


import sext._
case class QueryNode(tokenProperties : TokenQuery, children: Set[QueryNode] = Set()) {

  def nodeQuery(): Query = if (children.isEmpty) tokenProperties else {
    val clauses = children.map(x => x.nodeQuery()).map(x => Queries.headExtend(x))
    HeadRestrict(Queries.headIntersect(clauses.toSeq), tokenProperties)
  }

  def toCQL() : String = {
    val stukjes = children.map(x => x.toCQL()).mkString(" ")
    val tp = tokenProperties.toCQL()
    if (stukjes.isEmpty) s"[$tp]" else s"[$tp $stukjes]"
  }
}

// [geneste haakjes? [pos="VERB" & lemma="fietsen" [rel="obj"] [rel="nsubj"]]
// vergelijk https://ufal.mff.cuni.cz/pmltqdoc/doc/pmltq_tutorial_web_client.html

object QueryNode {
  // Ja dit kan ook compacter met @* enzo....
   def q(a: TokenQuery) = QueryNode(a)
   def q(a: TokenQuery,b: QueryNode) = QueryNode(a,Set(b))
   def q(a: TokenQuery,b: QueryNode, c: QueryNode) = QueryNode(a,Set(b,c))
   def q(a: TokenQuery,b: QueryNode, c: QueryNode, d: QueryNode) = QueryNode(a,Set(b,c,d))

  // [rel="root" [pos="NOUN" & rel="nsubj" [pos="ADJ" & lemma="Amerikaans"]] [pos="ADV"] ]
  val test: QueryNode =
    q(
      TokenRelQuery("root"),
      q(
        PoSQuery("NOUN") & TokenRelQuery("nsubj"),
        q(PoSQuery("ADJ") & LemmaQuery("Amerikaans"))
      ),
      q(PoSQuery("ADV"))
    )

  val test1: QueryNode = // [rel="root" [lemma="komen"] [rel="nsubj"] [lemma="uit" & rel="compound:prt"] [lemma="er"] ]
    q(
      TokenRelQuery("root") & LemmaQuery("komen"),
      q(TokenRelQuery("nsubj")),
      q(LemmaQuery("uit") & TokenRelQuery("compound:prt")),
      q(LemmaQuery("er"))
    )

   def testQuery(q: QueryNode) = {
     println(q.treeString.replaceAll("\\|", "\t"))
     println("Possible CQL+ serialization: " + q.toCQL())
     luchtfietsen.runQuery(q.nodeQuery())
   }

   def main(args: Array[String])  = {
     testQuery(test1)
   }
}
