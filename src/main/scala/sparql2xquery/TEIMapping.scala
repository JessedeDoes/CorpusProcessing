package sparql2xquery

import sparql2xquery.Mappings.PredicateMapping
import sparql2xquery.SparqlToXquery.parseResult

object TEIMapping {
  type PredicateMapping = Map[String, BasicPattern]
  val ontolex = "http://www.w3.org/ns/lemon/ontolex#"

  val testMap:PredicateMapping = Map(
    s"${ontolex}lexicalForm" ->
      BasicPattern(
        Set("subject←//entry/@id",
          "object←$subject//oVar/generate-id()")
      ),

    s"${ontolex}canonicalForm" ->
      BasicPattern(
        Set("subject←//entry/@id",
          "object←$subject//form[@type='lemma' and not ancestor::re]/generate-id()")
      ),

    s"${ontolex}writtenRep" ->
      BasicPattern(
        Set("subject←//form[@type='lemma'/generate-id()|//oVar/generate-id()",
          "object←$subject//text()")
      ),

    s"${ontolex}lexicalForm" ->
      BasicPattern(
        Set("subject←//node[@cat='smain']", "object←$subject/node[@rel='obj1']")
      ),
  )

  val teiMapping = TripleMapping(testMap)

  val q0 =
    s"""
       |prefix ontolex: <$ontolex>
       | select ?e ?l ?w where
       | {?e ontolex:canonicalForm ?cf .
       |   ?cf ontolex:writtenRep ?l .
       |   ?e ontolex:lexicalForm ?lf .
       |   ?lf ontolex:writtenRep ?w }
     """.stripMargin

  def main(args: Array[String]): Unit = {
    val t = new SparqlToXquery(teiMapping)
    val x = t.translate(q0)
    Console.err.println(x.toQuery())
    val bx = basex.BaseXConnection.wnt
    val resultStream = bx.getAsScalaNodes(x.toQuery()).map(parseResult)
    println(QueryResults.response(resultStream.take(1000)))
  }
}
