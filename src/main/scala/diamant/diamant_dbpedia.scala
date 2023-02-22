package diamant

import org.openrdf.model.Statement
import rdf.readRDF

object diamant_dbpedia {
  val nlpedia = "http://nl.dbpedia.org/sparql"
  def cross[T](X: Seq[T], Y:Seq[T]): Seq[(T, T)] = X.flatMap(x => Y.map(y => x -> y))

  val construct = """   ?dbpedia_resource a ?type .
                    |   ?dbpedia_resource dct:subject ?dbpedia_subject .
                    |   ?dbpedia_resource ontolex:isEvokedBy ?e .
                    |   ?dbpedia_resource diamant:historicalLabel ?term .
                    |   ?dbpedia_resource diamant:match ?match .
                    |   ?dbpedia_resource diamant:dbmatch ?dbmatch .
                    |""".stripMargin


  val prefixes = """PREFIX diamant:    <http://rdf.ivdnt.org/schema/diamant#>
                   |PREFIX lemon:  <http://lemon-model.net/lemon#>
                   |PREFIX lexinfo: <http://www.lexinfo.net/ontology/2.0/lexinfo#>
                   |PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
                   |PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
                   |PREFIX prov: <http://www.w3.org/ns/prov#>
                   |PREFIX ontolex: <http://www.w3.org/ns/lemon/ontolex#>
                   |PREFIX ud: <http://universaldependencies.org/u/pos/>
                   |PREFIX relation: <http://rdf.ivdnt.org/schema/diamant#relation/>
                   |PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
                   |PREFIX dct: <http://purl.org/dc/terms/>
                   |PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
                   |""".stripMargin

  val construct_dbpedia =
    """construct {
      |           ?dbpedia_resource a ?type .
      |           ?dbpedia_resource dct:subject ?dbpedia_subject .
      |           ?dbpedia_resource rdfs:label ?dbpedia_label .
      |}""".stripMargin

  val dbpedia_alternatives = List(
    "cat_aandoening" -> """                  {
                                    |            ?dbpedia_resource a ?type .
                                    |            ?dbpedia_resource dct:subject ?dbpedia_subject .
                                    |            ?dbpedia_subject skos:broader*  <http://nl.dbpedia.org/resource/Categorie:Aandoening> .
                                    |            ?dbpedia_resource rdfs:label ?dbpedia_label .
                                    |            bind('cat:aandoening' as ?dbmatch) .
                                    |#            optional { ?dbpedia_subject skos:broader* ?dbpedia_supercats }
                                    |         }""".stripMargin,

    "cat_ziekte" ->  """
                                    |         {
                                    |            ?dbpedia_resource a ?type .
                                    |            ?dbpedia_resource dct:subject ?dbpedia_subject .
                                    |            ?dbpedia_subject skos:broader*  <http://nl.dbpedia.org/resource/Categorie:Ziekte> .
                                    |            ?dbpedia_resource rdfs:label ?dbpedia_label .
                                    |            bind('cat:ziekte' as ?dbmatch) .
                                    |#            optional { ?dbpedia_subject skos:broader* ?dbpedia_supercats }
                                    |         } """.stripMargin,
    "onto_ziekte" -> """
                                    |         {
                                    |            ?dbpedia_resource a <http://dbpedia.org/ontology/Disease> .
                                    |            ?dbpedia_resource a ?type .
                                    |            ?dbpedia_resource dct:subject ?dbpedia_subject .
                                    |            ?dbpedia_resource rdfs:label ?dbpedia_label .
                                    |            bind('ontology:disease' as ?dbmatch) .
                                    |#            optional { ?dbpedia_subject skos:broader* ?dbpedia_supercats }
                                    |         } """.stripMargin)

  def constr(dbpedia_resource: String) = s"""
                 |   <$dbpedia_resource> ontolex:isEvokedBy ?e .
                 |   <$dbpedia_resource> diamant:historicalLabel ?term ."""

  def searchbyLemma(v: String, resource: String)  = {
    s"""$prefixes
       | construct {${constr(resource)}} where { graph ?g {
       |          ?e ontolex:canonicalForm ?form . ?form ontolex:writtenRep  ${v.replaceAll("@nl$","")} .
       |          ?e ontolex:sense ?sense .
       |          ?sense lemon:definition ?def . ?def a diamant:SynonymDefinition . ?def diamant:definitionText ?term .
       |          ?def a ?defType .
       |          bind('syn_from_lemma' as ?match)
       |       } } """.stripMargin
  }

  def searchbySyn(v: String, resource: String)  = {
    s"""$prefixes
       | construct {${constr(resource)}} where { graph ?g {
       |          ?def diamant:definitionText ${v.replaceAll("@nl$","")} .
       |          ?s lemon:definition ?def .
       |          ?e ontolex:sense ?s .
       |          ?e ontolex:canonicalForm ?cf .
       |          ?cf ontolex:writtenRep ?term
       |       } } """.stripMargin
  }

  def searchByLemmaWhere(v: String)  = {

    s"""{
      |          ?e ontolex:canonicalForm ?form . ?form ontolex:writtenRep  $v .
      |          ?e ontolex:sense ?sense .
      |          ?sense lemon:definition ?def . ?def a diamant:SynonymDefinition . ?def diamant:definitionText ?term .
      |          ?def a ?defType .
      |          bind('syn_from_lemma' as ?match)
      |       }""".stripMargin
  }
  val diamant_alternatives = List(
    "syn_from_lemma" ->   """{
                                    |          ?e ontolex:canonicalForm ?form . ?form ontolex:writtenRep  ?dbpedia_label_lowercase .
                                    |          ?e ontolex:sense ?sense .
                                    |          ?sense lemon:definition ?def . ?def a diamant:SynonymDefinition . ?def diamant:definitionText ?term .
                                    |          ?def a ?defType .
                                    |          bind('syn_from_lemma' as ?match)
                                    |       }""".stripMargin,
    "lemma_from_syn" ->   """
      |{
      |          ?def diamant:definitionText ?dbpedia_label_lowercase .
      |          ?sense lemon:definition ?def .
      |          ?e ontolex:sense ?sense .
      |          ?e ontolex:canonicalForm ?form .
      |          ?form ontolex:writtenRep  ?term .
      |          ?def a ?defType .
      |          bind('lemma_from_syn' as ?match)
      |       }
      |""".stripMargin)

   val combinations = cross(dbpedia_alternatives, diamant_alternatives).map(
     {
       case ((n1, dbpedia), (n2,diamant)) =>
         s"$n1 $n2" -> s"""$prefixes construct {
            | $construct
            |}  where
            | {
            |    graph  <http://rdf.ivdnt.org/lexica/diamant/v1.5/>
            |    {
            |      SERVICE  <$nlpedia>
            |      {
            |         $dbpedia
            |      }  .
            |      bind (lcase(xsd:string(?dbpedia_label)) as ?dbpedia_label_lowercase) .
            |      {
            |        $diamant
            |      }
            |    }
            | }
            |""".stripMargin
     })

  val stupid_query = prefixes + "\n" + "construct { ?s ?p ?o } where { graph ?g { ?s ?p ?o} }  limit 10 "
  val rekenserver = "http://gpuding:8085/fuseki/tdb/sparql"

  def main(args: Array[String]) = {
    val dbpedia_statements = dbpedia_alternatives.flatMap({case (n,a) => {
      val q =
        s"""$prefixes
           |$construct_dbpedia
           |where $a
           |""".stripMargin
       // println(q)
       val s = readRDF.remoteGraphQuery(nlpedia,q).toSet
       println(s"$n -> ${s.size}")
       s
    }})

    println(dbpedia_statements.size)

    val dbpedia_terms = dbpedia_statements.filter(_.getPredicate.toString.contains("label"))

    dbpedia_terms.foreach(s => {
      val label = s.getObject
      val subject  = s.getSubject
      // println(label)
      val q = searchbySyn(label.toString.toLowerCase(), subject.toString) // s"""$prefixes construct { <$subject> rdfs:label ?term} where ${searchByLemmaWhere(label.toString.toLowerCase())}"""
      // println(q)
      val x0 = readRDF.remoteGraphQuery(rekenserver,q).toSet

      val l = label.toString.toLowerCase() // ("\"kekeren\"@nl"
      val q1 = searchbyLemma(l, subject.toString)
      //println(q1)
      val x1 = readRDF.remoteGraphQuery(rekenserver,q1).toSet

      val x: Set[Statement] = x0 ++ x1
      if (x.nonEmpty)
      {
        // println(q)
        println(s"####### $label #####")
        x.foreach(println)
      }
    })
    System.exit(0)




    println(stupid_query)
    readRDF.remoteGraphQuery(rekenserver,stupid_query).foreach(println)
    combinations.foreach({ case (n,c) => {
      println(s"\n\n#### $n ####")
      println(c)
      readRDF.remoteGraphQuery(rekenserver,c).foreach(println)
    }})
  }
}
