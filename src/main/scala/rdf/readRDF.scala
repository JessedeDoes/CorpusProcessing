package rdf

import java.io.{FileInputStream, PrintStream, PrintWriter, StringReader}
import java.util.zip.GZIPInputStream

import guru.nidi.graphviz.engine.{Format, Graphviz}
import guru.nidi.graphviz.parse._
import org.openrdf.model._
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

import scala.annotation.tailrec
import org.openrdf.model.Model
import org.openrdf.rio.nquads.NQuadsWriter
import org.openrdf.rio.turtle.TurtleWriter


object readRDF {
  val formatsToTry = List(RDFFormat.TURTLE, RDFFormat.RDFXML, RDFFormat.NTRIPLES, RDFFormat.N3, RDFFormat.NQUADS, RDFFormat.JSONLD, RDFFormat.RDFJSON)

  val rdfParsers = formatsToTry.map(Rio.createParser(_)).toStream

  //val rdfParser: RDFParser = Rio.createParser(RDFFormat.TURTLE)

  def parseToGraph(f: java.io.File): Graph = parseToGraph(() => {
    val i = new FileInputStream(f)
    if (f.getName.endsWith(".gz")) new GZIPInputStream(i) else i
  })

  def parseToGraph(fileName: String): Graph = parseToGraph(new java.io.File(fileName))

  def parseStringToGraph(s: String): Graph = {
    def inputStream(): java.io.InputStream = {
      val sr = new StringReader(s)

      val x = new ReaderInputStream(sr)
      x
    }

    parseToGraph(() => inputStream)
  }

  def parseToGraph(inputStream: () => java.io.InputStream): org.openrdf.model.Graph = {
    import org.openrdf.rio.helpers.StatementCollector

    val attempts = rdfParsers.map(
      rdfParser => {
        Try({
          val myGraph = new org.openrdf.model.impl.GraphImpl

          val collector = new StatementCollector(myGraph)

          rdfParser.setRDFHandler(collector)
          rdfParser.parse(inputStream(), "http://")

          myGraph
        })
      })
    attempts.filter(_.isFailure).map(_.asInstanceOf[scala.util.Failure[Graph]]).foreach(println)
    attempts.find(_.isSuccess).map(_.asInstanceOf[scala.util.Success[Graph]].value).get
  }


  def parseToStatements(url: String): Stream[Statement] = parseToGraph(url).iterator().asScala.toStream

  def parseStringToStatements(rdf: String): Stream[Statement] = parseStringToGraph(rdf).iterator().asScala.toStream

  def lcp(list: String*) = list.foldLeft("")((_,_) =>
    (list.min.view,list.max.view).zipped.takeWhile(v => v._1 == v._2).unzip._1.mkString)

  def uniqueSuffix(all: Set[String], s: String) = {
    val maxCommonSufLen = all.filter(_ != s).map(z => lcp(s.reverse, z.reverse).length).max
    val start = s.length - maxCommonSufLen-1
    val x = s.zipWithIndex.filter({case (c,i) => (c== '/' || c == '#') && i < start}).lastOption.map(_._2)
    if (x.isDefined) {
      val z = s.substring(x.get, s.length)
      Console.err.println(s"HOO $start -> $x ($z)")
      z
    } else s.substring(start,s.length)
  }

  def getShortNameMapR(r: Set[org.openrdf.model.Resource]): Map[Value, String] = {
    val allNames = r.map(_.stringValue())
    r.map(r => r -> uniqueSuffix(allNames, r.stringValue())).toMap
  }

  def getShortNameMapV(r: Set[org.openrdf.model.Value]): Map[Value, String] = {
     val allNames = r.map(_.stringValue())
     r.map(r => r -> uniqueSuffix(allNames, r.stringValue())).toMap
  }
  def getShortNameMapU(r: Set[org.openrdf.model.URI]): Map[URI, String] = {
    val allNames = r.map(_.stringValue())
    r.map(r => r -> uniqueSuffix(allNames, r.stringValue())).toMap
  }
  // Bug: short name moet uniek gemaakt worden ...
  def shortName(r: org.openrdf.model.Value): String = {
    val s = r.stringValue().replaceAll("[#/]$", "").replaceAll(".*(#|/)", "")
    val x = if (s.isEmpty) r.stringValue().replaceAll("^[A-Za-z:]", "") else s
    Console.err.println(s"${r.stringValue()} --> $x")
    x
  }

  def shortName(r: org.openrdf.model.URI): String = {
    val s = r.stringValue().replaceAll("[#/]$", "").replaceAll(".*(#|/)", "")
    if (s.isEmpty) r.stringValue().replaceAll("^[A-Za-z:]", "") else s
  }


  def isObjectProperty(st: Statement): Boolean = st.getObject.isInstanceOf[org.openrdf.model.Resource]

  def isDataProperty(st: Statement): Boolean = st.getObject.isInstanceOf[org.openrdf.model.Literal]


  // def toStream[A](iter: Iterator[A]) = Stream.unfold(iter)(i=>if(i.hasNext) Some((i.next,i)) else None)

  // stack overflow als te vaak aangeroepen

  def toStream(r: GraphQueryResult): Stream[Statement] = {
    if (r.hasNext)
      Stream.cons(r.next(), toStream(r))
    else
      Stream.empty[Statement]
  }

  def toStream(r: TupleQueryResult): Stream[BindingSet] = {
    if (r.hasNext)
      Stream.cons(r.next(), toStream(r))
    else
      Stream.empty[BindingSet]
  }


  def remoteGraphQuery(endpoint: String, query: String, credentials: Option[(String,String)] = None): Stream[Statement] = {
    val repo = new HTTPRepository(endpoint)
    credentials.foreach{case (u,p) => repo.setUsernameAndPassword(u, p)}
    val con = repo.getConnection
    val q = con.prepareGraphQuery(
      QueryLanguage.SPARQL, query)

    q.setIncludeInferred(false)

    val graphResult: GraphQueryResult = q.evaluate()

    toStream(graphResult)
  }

  def remoteTupleQuery(endpoint: String, query: String, credentials: Option[(String,String)] = None): Seq[BindingSet] = {

    import org.openrdf.repository.Repository
    import org.openrdf.repository.http.HTTPRepository

    val repo = new HTTPRepository(endpoint) // er kan nog een repoID bij, wwaarschijnlijk nodig voor onze virtuoso?
    credentials.foreach{case (u,p) => repo.setUsernameAndPassword(u, p)}

    var res= Stream.empty[BindingSet]
    try {
      val con = repo.getConnection
      try {
        val tupleQuery = con.prepareTupleQuery(QueryLanguage.SPARQL, query)
        val iterator: TupleQueryResult = tupleQuery.evaluate()
        //println(iterator.getBindingNames + " " + iterator.hasNext + " " + iterator.next())


        val result = toStream(iterator)
        //val zz = result.toList
        //println(zz)
        res = result
      }
      finally {
        con.close();
      }
    }
    catch {
      case e: Exception => e.printStackTrace()
    }
    res
  }

  val dbpedia = "https://dbpedia.org/sparql"

  def testje(): Unit = {

    val queryString = "SELECT ?x ?y WHERE { ?x ?p ?y . ?x a skos:Concept }  limit 10000 "

    remoteTupleQuery(dbpedia, queryString).foreach(bindingSet => {
      val valueOfX = bindingSet.getValue("x")
      val valueOfY = bindingSet.getValue("y")
      Console.err.println(s"YOO: $valueOfX $valueOfY")
    })
  }

  /*
  Triplestore: http://142.93.226.251:10035/#
User: ivdnt
Pass: Ui8UytKiG0QDo8j

   */

  val evoke0 = "http://142.93.226.251:10035/repositories/evoke?queryLn=SPARQL&infer=false&uuid=yvds3u8bd2g7cnvjb665ap&returnQueryMetadata=true&checkVariables=false"
  val evoke2 = "http://142.93.226.251:10035/#"
  val evoke3 = "http://142.93.226.251:10035/repositories/evoke?queryLn=SPARQL&checkVariables=false"


  def testEvoke = {
    val nTriples = 575368
    val bunch = 5000

    val allStatements: Stream[Statement] = (0 until nTriples by bunch).toStream.flatMap(i => {
      val queryString = s"construct  { ?s ?p ?o  } where { graph ?g { ?s ?p ?o . }  } limit $bunch offset $i"
      Console.err.println(queryString)
      val z = remoteGraphQuery(evoke0, queryString, Some(("ivdnt", "Ui8UytKiG0QDo8j")))
      if (false) z.foreach(s => {
        println(s)
      })
      z
    })
    rdf2db.insertToTable(evoke.evoke_db, "data.evoke", allStatements)
    quadIt(allStatements, new PrintStream("/tmp/evoke.nq"))
    turtleIt(allStatements, new PrintStream("/tmp/evoke.ttl"))

  }

  def testjeG() = { // inference uit!

    val queryString = "CONSTRUCT { ?x ?p ?y } WHERE { ?x ?p ?y . ?x a skos:Concept }  limit 10 "
    remoteGraphQuery(dbpedia, queryString).foreach(println)
  }

  def stopLogback() = {

    import ch.qos.logback.classic.LoggerContext
    /*
      import org.slf4j.LoggerFactory
    import ch.qos.logback.classic.Level
    import ch.qos.logback.classic.Logger
    val loggers = Seq(
      "org.apache.http",
      "groovyx.net.http",
      "oh ja"
    )

    loggers.foreach { name =>
      val logger = LoggerFactory.getLogger(name)
      if (logger.isInstanceOf[Logger]) {
        val l1 = logger.asInstanceOf[Logger]
        l1.setLevel(Level.WARN)
        l1.setAdditive(false)
      } else
        {
          Console.err.println(logger)
        }
    }
    */
    import org.slf4j.LoggerFactory
    val loggerContext = LoggerFactory.getILoggerFactory.asInstanceOf[LoggerContext]
    loggerContext.stop()
    //System.exit(1)
  }

  //stopLogback()

  def collectStuff(endpoint: String, i: String, depth: Int): Stream[Statement] = {
    if (depth <= 0) Stream.empty[Statement]
    else {
      Console.err.println(depth)
      val q = s"construct { <$i> ?p ?o } where { <$i> ?p ?o} limit 10"
      //Console.err.println(q)
      val s = remoteGraphQuery(endpoint, q)
      //s.zipWithIndex.foreach(x => Console.err.println(x))
      val objects = s.filter(isObjectProperty).map(_.getObject.asInstanceOf[Resource])
      val down = objects.flatMap(r => collectStuff(endpoint, r.toString, depth - 1))
      s ++ down
    }
  }

  val huw = "http://dbpedia.org/resource/Horse_Under_Water"

  def main(args: Array[String]): Unit = {
    testEvoke
    System.exit(0)
    //org.apache.logging.log4j.LogManager.get
    val s = collectStuff(dbpedia, huw, 4)
    println(s.size)
    import org.openrdf.rio.RDFFormat
    import org.openrdf.rio.RDFWriter
    import org.openrdf.rio.Rio
    val writer = Rio.createWriter(RDFFormat.NQUADS, System.out)
    writer.startRDF
    s.toSet.foreach(st => writer.handleStatement(st))
    writer.endRDF
    // connecties moeten worden afgesloten -- hoe netjes te doen?
    //testjeG()
  }

  def turtleIt(s: Seq[Statement], writer: PrintStream = System.out) = {
    val tw: TurtleWriter = new TurtleWriter(writer)
    tw.startRDF()
    s.foreach(st => { tw.handleStatement(st) } )
    tw.endRDF()
    writer.close()
  }

  def quadIt(s: Seq[Statement], writer: PrintStream = System.out) = {
    val q = new NQuadsWriter(writer)
    q.startRDF()
    s.foreach(st => { q.handleStatement(st) } )
    q.endRDF()
    writer.close()
  }
}

/*
import org.openrdf.model.Model
import org.openrdf.rio.turtle.TurtleWriter
import java.util
def ttlToStdout(model: Model): Unit =  { val tw: TurtleWriter = new TurtleWriter(System.out)
try {tw.startRDF()
import scala.collection.JavaConversions._
for (en <- DEFAULTNAMESPACES.entrySet)  { tw.handleNamespace(en.getKey, en.getValue)
}
import scala.collection.JavaConversions._
for (s <- model)  { tw.handleStatement(s)
}
tw.endRDF()} catch {
case e: Exception =>
log.error(e, e)
}
}

public static void ttlToStdout( Model model ) {
	TurtleWriter tw = new TurtleWriter( System.out );
	try {
		tw.startRDF();
		for ( Map.Entry<String, String> en : DEFAULTNAMESPACES.entrySet() ) {
			tw.handleNamespace( en.getKey(), en.getValue() );
		}

		for ( Statement s : model ) {
			tw.handleStatement( s );
		}
		tw.endRDF();
	}
	catch ( Exception e ) {
		log.error( e, e );
	}
}
 */
