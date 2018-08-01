package rdf

import java.io.{FileInputStream, StringReader}
import java.util.zip.GZIPInputStream

import guru.nidi.graphviz.engine.{Format, Graphviz}
import guru.nidi.graphviz.parse._
import org.openrdf.model.{Graph, Resource, Statement}
import org.openrdf.rio.{RDFFormat, Rio}
import org.postgresql.util.ReaderInputStream
import rdf.Settings.prefixMap
import utils.PostProcessXML

import scala.collection.JavaConverters._
import scala.util.Try
import scala.xml._

import org.openrdf.repository.Repository
import org.openrdf.repository.http.HTTPRepository
import org.openrdf.query.TupleQuery
import org.openrdf.query.TupleQueryResult
import org.openrdf.query.BindingSet
import org.openrdf.query.QueryLanguage
import org.openrdf.query.GraphQueryResult


import org.apache.commons.logging.Log
import org.apache.commons.logging.LogFactory

object readRDF
{
  val formatsToTry = List(RDFFormat.TURTLE, RDFFormat.RDFXML, RDFFormat.NTRIPLES, RDFFormat.N3, RDFFormat.NQUADS,  RDFFormat.JSONLD, RDFFormat.RDFJSON)

  val rdfParsers = formatsToTry.map(Rio.createParser(_)).toStream

  //val rdfParser: RDFParser = Rio.createParser(RDFFormat.TURTLE)

  def parseToGraph(f: java.io.File):Graph = parseToGraph(() =>
  {
    val i = new FileInputStream(f)
    if (f.getName.endsWith(".gz")) new GZIPInputStream(i) else i
  } )

  def parseToGraph(fileName: String):Graph = parseToGraph(new java.io.File(fileName))

  def parseStringToGraph(s: String):Graph =
  {
    def inputStream():java.io.InputStream = {
      val sr = new StringReader(s)

      val x = new ReaderInputStream(sr)
      x
    }

    parseToGraph(() => inputStream)
  }

  def parseToGraph(inputStream: ()=>java.io.InputStream):org.openrdf.model.Graph  = {
    import org.openrdf.rio.helpers.StatementCollector

    val attempts = rdfParsers.map(
      rdfParser => {
        Try({
        val myGraph = new org.openrdf.model.impl.GraphImpl

        val collector = new StatementCollector(myGraph)

        rdfParser.setRDFHandler(collector)
        rdfParser.parse(inputStream(), "http://")

        myGraph})
      })
    attempts.filter(_.isFailure).map(_.asInstanceOf[scala.util.Failure[Graph]]).foreach(println)
    attempts.find(_.isSuccess).map(_.asInstanceOf[scala.util.Success[Graph]].value).get
  }


  def parseToStatements(url: String): Stream[Statement] = parseToGraph(url).iterator().asScala.toStream
  def parseStringToStatements(rdf: String): Stream[Statement] = parseStringToGraph(rdf).iterator().asScala.toStream

  def shortName(r: org.openrdf.model.Value) = {
    val s = r.stringValue().replaceAll("[#/]$","").replaceAll(".*(#|/)", "")
    if (s.isEmpty) r.stringValue().replaceAll("^[A-Za-z:]","") else s
  }

  def shortName(r:  org.openrdf.model.URI) = {
    val s = r.stringValue().replaceAll("[#/]$","").replaceAll(".*(#|/)", "")
    if (s.isEmpty) r.stringValue().replaceAll("^[A-Za-z:]","") else s
  }


  def isObjectProperty(st: Statement) = st.getObject.isInstanceOf[org.openrdf.model.Resource]
  def isDataProperty(st: Statement) = st.getObject.isInstanceOf[org.openrdf.model.Literal]


  def toStream(r: GraphQueryResult):Stream[Statement] =
  {
    if (r.hasNext)
      Stream.cons(r.next(), toStream(r))
    else
      Stream.empty[Statement]
  }

  def toStream(r: TupleQueryResult):Stream[BindingSet] =
  {
    if (r.hasNext)
      Stream.cons(r.next(), toStream(r))
    else
      Stream.empty[BindingSet]
  }


   def remoteGraphQuery(endpoint: String, query: String):Stream[Statement] =
   {
     val repo = new HTTPRepository(endpoint)

     val con = repo.getConnection()
     val q = con.prepareGraphQuery(
       QueryLanguage.SPARQL, "CONSTRUCT { ?s ?p ?o } WHERE {?s ?p ?o . ?o a skos:Concept} limit 100")

     q.setIncludeInferred(false)

     val graphResult: GraphQueryResult  = q.evaluate()

     toStream(graphResult)
   }

   def remoteTupleQuery(endpoint: String, query: String):Stream[BindingSet] =
   {
     import org.openrdf.repository.Repository
     import org.openrdf.repository.http.HTTPRepository

     val repo = new HTTPRepository(endpoint) // er kan nog een repoID bij, wwaarschijnlijk nodig voor onze virtuoso?

     Console.err.println(LogFactory.getFactory.getClass)
     System.exit(1)

     try {
       val con = repo.getConnection()
       try {
         val tupleQuery = con.prepareTupleQuery(QueryLanguage.SPARQL, query)
         val result= toStream(tupleQuery.evaluate())
         result
       }
       finally {
         con.close();
       }
     }
     catch  {
       case e: Exception => e.printStackTrace()
     }
     Stream.empty
   }


  def testje(): Unit =
  {
    val sesameServer = "https://dbpedia.org/sparql"
    val queryString = "SELECT ?x ?y WHERE { ?x ?p ?y . ?x a skos:Concept }  limit 10000 "

    remoteTupleQuery(sesameServer, queryString).foreach(bindingSet =>
    {
      val valueOfX = bindingSet.getValue("x")
      val valueOfY = bindingSet.getValue("y")
      Console.err.println(s"YOO: $valueOfX $valueOfY")
    })
  }

  def testjeG() = {  // inference uit!
    val sesameServer = "https://dbpedia.org/sparql"
    val queryString = "CONSTRUCT { ?x ?p ?y } WHERE { ?x ?p ?y . ?x a skos:Concept }  limit 10 "
    remoteGraphQuery(sesameServer, queryString).foreach(println)
  }

  def main(args: Array[String]): Unit = {
    testjeG()
  }
}


