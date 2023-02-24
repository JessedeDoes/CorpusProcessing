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
  val queries: Seq[String] = List("dbpedia_bycat.sparql",  "dbpedia_bytype.sparql").map(queryDir + _)

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

  def main(args: Array[String]) = {
    val w = new PrintWriter("/tmp/dbpedia_nl_medical.nq")
    val wt = new PrintWriter("/tmp/dbpedia_nl_medical.tsv")
    results.foreach(s =>  { w.println(stmtToString(s)); wt.println(stmtToTSV(s)) } )
    w.close()
    wt.close()
  }
}
