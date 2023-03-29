package corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.syntax_poc


import sext._
case class QueryNode(tokenProperties : TokenQuery, children: Set[QueryNode] = Set()) {
  def nodeQuery(): Query = if (children.isEmpty) tokenProperties else {
    val clauses = children.map(x => x.nodeQuery()).map(x => Queries.headExtend(x))
    HeadRestrict(Queries.headIntersect(clauses.toSeq), tokenProperties)
  }
}

object QueryNode {
   val test = QueryNode(
       TokenRelQuery("root") ,
       Set(
         QueryNode(PoSQuery("NOUN") & TokenRelQuery("nsubj"),
           Set(
             QueryNode(PoSQuery("ADJ") & LemmaQuery("Amerikaans"))
           )
         ),
         QueryNode(PoSQuery("ADV"))
      )
   )

   def main(args: Array[String])  = {
     println(test.treeString)

     luchtfietsen.runQuery(test.nodeQuery())
   }
}
