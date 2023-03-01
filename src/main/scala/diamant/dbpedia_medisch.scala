package diamant
import org.openrdf.model.Statement
import rdf.readRDF
import org.openrdf.model.impl.URIImpl

import java.io.PrintWriter
import scala.collection.immutable

object dbpedia_medisch {
  val nlpedia = "http://nl.dbpedia.org/sparql"

  def queryFromFile(url: String, filename: String): immutable.Seq[Statement] = {
    val content = io.Source.fromFile(filename).getLines().mkString("\n")
    readRDF.remoteGraphQuery(url ,content)
  }

  val queryDir = "/home/jesse/workspace/friedabundel/scripts/Query/Ziekte/"
  val queries: Seq[String] = List("dbpedia_bycat.sparql",  "dbpedia_bycat_geneesk.sparql", "dbpedia_bytype.sparql").map(queryDir + _)

  lazy val results: Seq[Statement] = queries.flatMap(q => queryFromFile(nlpedia, q)).distinct.sortBy[String](s => s.getSubject.toString)

  def stmtToString(s: Statement): String = {
    val o = s.getObject
    val os = if (o.isInstanceOf[URIImpl]) s"<$o>" else o.toString
    s"<${s.getSubject}> <${s.getPredicate}> $os <http://nl.dbpedia.org/> . "
  }

  def stmtToTSV(s: Statement): String = {
    val o = s.getObject
    val os = if (o.isInstanceOf[URIImpl]) o.toString else o.toString.replaceAll("@[a-z]+$","").replaceAll(""""""", "")
    s"${s.getSubject}\t${s.getPredicate}\t$os"
  }

  val prefixes =  s"""
         |PREFIX diamant:    <http://rdf.ivdnt.org/schema/diamant#>
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
         |PREFIX dbo: <http://dbpedia.org/ontology/>""".stripMargin

  def portionCategory(k: Int) = {
    System.err.println(s"Portion $k")
    val q =
      s"""
         $prefixes
         |
         |construct {
         |  ?c skos:broader ?parent
         |}
         |where {
         |?c rdf:type skos:Concept .
         |?c skos:broader*  <http://nl.dbpedia.org/resource/Categorie:Geneeskunde> .
         |?c skos:broader ?parent
         |} offset ${k * 5000} limit 5000
         |""".stripMargin
    try {
      readRDF.remoteGraphQuery(nlpedia , q)
    } catch {
      case e =>
        System.err.println(s"Failed at portion $k!")
        Stream[Statement]();
    }
  }

  def  getFromNLPedia(q: String) =  {
    val t = readRDF.remoteGraphQuery(nlpedia , q).toSet
    System.err.println(s"Got ${t.size} statements")
    t
  }

  def qType(k: Int)  = {
    s"""
       $prefixes
       |
       |construct {
       |  ?r dct:subject ?cat .
       |  ?r rdfs:label ?label .
       |  ?r rdf:type ?t .
       |  ?r diamant:alt ?redir_label
       |}
       |where {
       | ?r rdf:type ?t .
       | ?r rdfs:label ?label .
       | values ?t { <http://dbpedia.org/ontology/Disease> } .
       | optional {   ?r dct:subject ?cat . } .
       | optional {  ?redirected <http://dbpedia.org/ontology/wikiPageRedirects> ?r  .
       |   ?redirected rdfs:label ?redir_label
       | } .
       |} offset ${k * 5000} limit 5000
       |""".stripMargin
  }
  def qCategory(k: Int, root: String) =
    s"""
       $prefixes
       |
       |construct {
       |  ?r dct:subject ?cat .
       |  ?r rdfs:label ?label .
       |  ?r rdf:type ?t .
       |  ?r diamant:alt ?redir_label
       |}
       |where {
       | ?r dct:subject ?cat .
       | ?cat skos:broader* ?roots .
       | ?r rdfs:label ?label .
       | values ?roots { <http://nl.dbpedia.org/resource/Categorie:$root> } .
       | optional {  ?r rdf:type ?t} .
       | optional {  ?redirected <http://dbpedia.org/ontology/wikiPageRedirects> ?r  .
       |   ?redirected rdfs:label ?redir_label
       | } .
       |} offset ${k * 5000} limit 5000
       |""".stripMargin

  def portionResources(k: Int) = {
    System.err.println(s"Portion $k")
    try {
      getFromNLPedia(qCategory(k,"Ziekte")) ++  getFromNLPedia(qCategory(k,"Aandoening")) ++ getFromNLPedia(qType(k))
    } catch {
      case e =>
        System.err.println(s"Failed at portion $k!: " + e)
        Set[Statement]();
    }
  }

  def queryByCatAndType() = {
    val w = new PrintWriter("/tmp/dbpedia_nl_medical.nq")
    val wt = new PrintWriter("/tmp/dbpedia_nl_medical.tsv")
    results.foreach(s =>  {
      w.println(stmtToString(s));
      wt.println(stmtToTSV(s))
    })
    w.close()
    wt.close()
  }

  def findCategoriesBelowGeneeskunde() = {
    val w = new PrintWriter("/tmp/dbpedia_categories_below_medicine.nq")
    val allStatements = (0 until 100).iterator.flatMap(k => portionCategory(k)).toSet
    println(allStatements.size)
    allStatements.foreach(s => w.println(stmtToTSV(s)))
    w.close()
    System.exit(0)
    // allStatements.foreach(println)
  }

  def findAllInGeneeskunde() = {
    val w = new PrintWriter("/tmp/dbpedia_resources_below_medicine.tsv")
    val allStatements = (0 until 200).iterator.map(portionResources).takeWhile(_.nonEmpty).flatten.toSet
    println(allStatements.size)
    allStatements.foreach(s => w.println(stmtToTSV(s)))
    w.close()
    System.exit(0)
  }

  def main(args: Array[String]) = {
    println(prefixes)
    findAllInGeneeskunde()
  }
}
