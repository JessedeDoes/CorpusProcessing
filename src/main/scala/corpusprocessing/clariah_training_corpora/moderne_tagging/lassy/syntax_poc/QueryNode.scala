package corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.syntax_poc


import sext._
case class QueryNode(tokenProperties : TokenQuery, children: Set[QueryNode] = Set()) {

  def nodeQuery(): Query = if (children.isEmpty) tokenProperties else {
    val clauses = children.map(x => x.nodeQuery()).map(x => Queries.headExtend(x))
    HeadRestrict(Queries.headIntersect(clauses.toSeq), tokenProperties)
  }
}

// [geneste haakjes? [pos="VERB" & lemma="fietsen" [rel="obj"] [rel="nsubj"]]
// vergelijk https://ufal.mff.cuni.cz/pmltqdoc/doc/pmltq_tutorial_web_client.html

object QueryNode {
   def N(a: TokenQuery) = QueryNode(a)
   def N(a: TokenQuery,b: QueryNode) = QueryNode(a,Set(b))
   def N(a: TokenQuery,b: QueryNode, c: QueryNode) = QueryNode(a,Set(b,c))

  val test: QueryNode = N(
       TokenRelQuery("root") ,
         N(
             PoSQuery("NOUN") & TokenRelQuery("nsubj"),
                N(PoSQuery("ADJ") & LemmaQuery("Amerikaans"))
         ),
         N(PoSQuery("ADV"))
   )

   def main(args: Array[String])  = {
     println(test.treeString)
     luchtfietsen.runQuery(test.nodeQuery())
   }
}
