package sparql2xquery

import sparql2xquery.SparqlToXquery.parseResult

object TestSparql2XQuery {
  def main(args: Array[String]):Unit =
  {
    val t = new SparqlToXquery(Mappings.testje)
    val q =
      s"""prefix : <http://example.org/>
         |prefix ud: <${Mappings.udPrefix}>
         |select ?s  ?o ?vl where
         |{
         | ?x :rel_su ?s .
         | ?s :lemma "mens" .
         | ?x :rel_obj1 ?o .
         | ?o :lemma ?w .
         | values ?w  {"zich"} .
         | ?x :child ?c .
         | ?c :pos "verb" .
         | ?c :lemma ?vl
         | }
      """.stripMargin

    val q1 =
      s"""prefix : <http://example.org/>
         |prefix ud: <${Mappings.udPrefix}>
         |select ?tonderwerp ?tgezegde ?tlv ?tmv where
         |{
         | ?gezegde ud:nsubj ?onderwerp .
         | ?gezegde ud:obj ?lv .
         | ?gezegde ud:iobj ?mv .
         |
         | ?gezegde :text ?tgezegde .
         | ?onderwerp :text ?tonderwerp .
         | ?lv :text ?tlv .
         | ?mv :text ?tmv
         | }
      """.stripMargin

    val q2 =  s"""prefix : <http://example.org/>
                 |prefix ud: <${Mappings.udPrefix}>
                 |select ?t1 ?t2 ?tcc where
                 |{
                 | ?c1 ud:conj ?c2 .
                 | ?c2 ud:cc ?cc .
                 | ?c1 :text ?t1 .
                 | ?c2 :text ?t2 .
                 | ?cc :text ?tcc
                 |}
      """.stripMargin

    val q3 = s"""prefix : <http://example.org/>
                |prefix ud: <${Mappings.udPrefix}>
                |select ?t1 ?t2 ?t3 where
                |{
                | ?c1 ud:xcomp ?c2 .
                | ?c1 :text ?t1 .
                | ?c2 :text ?t2 .
                | ?c2 :child ?ssub .
                | ?ssub ud:xcomp ?c3 .
                | ?c3 :text ?t3
                |}
      """.stripMargin

    val q4 = s"""prefix : <http://example.org/>
                |prefix ud: <${Mappings.udPrefix}>
                |select ?t1 ?t2  where
                |{
                | ?c1 ud:ccomp ?c2 .
                | ?c1 :text ?t1 .
                | ?c2 :text ?t2
                | }
      """.stripMargin


    val q5 =
      s"""prefix : <http://example.org/>
         |prefix ud: <${Mappings.udPrefix}>
         |select ?tonderwerp ?tgezegde ?tlv ?sent where
         |{
         | ?gezegde ud:nsubj ?onderwerp .
         | ?gezegde ud:obj ?lv .
         | ?lv :precedes ?onderwerp .
         | ?gezegde :text ?tgezegde .
         | ?onderwerp :text ?tonderwerp .
         | ?lv :text ?tlv .
         | ?gezegde :sentence ?sent
         | }
      """.stripMargin

    println(q5)

    val q6 =
      s"""
         |prefix ud: <${Mappings.udPrefix}>
         |SELECT *
         |WHERE {
         |
        |      ?pred ud:nsubj ?onderwerp .
         |
        |}
         |
      """.stripMargin
    val x:XQueryNode = t.translate(q5)
    println(x)
    println(x.toQuery())
    val bx = basex.BaseXConnection.default()
    val resultStream = bx.getAsScalaNodes(x.toQuery()).map(parseResult)
    println(QueryResults.response(resultStream.take(1000)))
    //bx.ge.tAsScalaNodes(x.toQuery()).foreach(n => QueryResults.toJSON(parseResult(n)));
  }
}
