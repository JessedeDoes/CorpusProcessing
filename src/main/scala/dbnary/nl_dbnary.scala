package dbnary
import rdf.rdf2db.{createSchema, insertToTable}
import rdf.readRDF.{parseToStatements, parseWithHandler}

object nl_dbnary {
  val wiktionary = "/mnt/Projecten/Taalbank/Definitiebank/Data/Dbnary/nl_dbnary_ontolex.ttl"


  def main(args: Array[String]): Unit = {
    parseWithHandler(wiktionary, s => {
      if (Set("ontolex", "definition", "skos:example", "lexinfo", "rdf:value").exists(s.getPredicate.toString.contains(_))) {
        Console.err.println(s)
      }
    })
  }
}
