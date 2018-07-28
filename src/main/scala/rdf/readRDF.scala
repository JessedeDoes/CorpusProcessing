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

object Settings
{
  val prefixes =  s"""
                     |@prefix celex: <http://ws-boelhouwer:8080/fuseki/celex/> .
                     |@prefix dc: <http://purl.org/dc/elements/1.1/#> .
                     |@prefix dcterms: <http://dublincore.org/2012/06/14/dcterms.ttl#> .
                     |@prefix dcterms: <http://purl.org/dc/terms/> .
                     |@prefix decomp: <http://www.w3.org/ns/lemon/decomp#> .
                     |@prefix diamant: <http://rdf.ivdnt.org/schema/diamant#> .
                     |@prefix fabio: <http://purl.org/spar/fabio/> .
                     |@prefix gold: <http://www.w3.org/ns/gold#> .
                     |@prefix int: <http://rdf.ivdnt.org/schema/anw/> .
                     |@prefix intskos: <http://ws-boelhouwer:8080/fuseki/intskos/> .
                     |@prefix lemon: <http://lemon-model.net/lemon#> .
                     |@prefix lexinfo: <http://www.lexinfo.net/ontology/2.0/lexinfo> .
                     |@prefix lexinfo: <http://www.lexinfo.net/ontology/2.0/lexinfo#> .
                     |@prefix lmf: <http://www.lexinfo.net/lmf> .
                     |@prefix madsrdf: <http://www.loc.gov/mads/rdf/v1#> .
                     |@prefix nif: <http://persistence.uni-leipzig.org/nlp2rdf/ontologies/nif-core#> .
                     |@prefix olia: <http://www.w3.org/ns/olia#> .
                     |@prefix olia_top: <http://purl.org/olia/olia-top.owl> .
                     |@prefix olio: <http://www.w3.org/ns/olia#> .
                     |@prefix ontolex: <http://www.w3.org/ns/lemon/ontolex#> .
                     |@prefix owl: <http://www.w3.org/2002/07/owl#> .
                     |@prefix prov: <http://www.w3.org/ns/prov#> .
                     |@prefix pwn: <http://wordnet-rdf.princeton.edu/ontology#> .
                     |@prefix quest: <http://obda.org/quest#> .
                     |@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
                     |@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
                     |@prefix skos: <http://www.w3.org/2004/02/skos/core#> .
                     |@prefix synsem: <http://www.w3.org/ns/lemon/synsem#> .
                     |@prefix ud: <http://universaldependencies.org/u/> .
                     |@prefix vartrans: <http://www.w3.org/ns/lemon/vartrans#> .
                     |@prefix wordnet: <http://wordnet-rdf.princeton.edu/wn31/> .
                     |@prefix xml: <http://www.w3.org/XML/1998/namespace> .
                     |@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
                     |@prefix xs: <http://www.w3.org/2001/XMLSchema#> .
                     |@prefix odp: <http://www.ontologydesignpatterns.org/cp/owl/semiotics.owl#> .
                     |@prefix voaf: <http://purl.org/vocommons/voaf#> .
                     |@prefix dcterms: <http://dublincore.org/2012/06/14/dcterms.ttl#> .
                     |@prefix : <#> .\n""".stripMargin


  lazy val prefixMap:Map[String,org.semanticweb.owlapi.model.IRI] = prefixes.split("\n").filter(_.contains("@")).toStream.map(l => {
     val l1 = l.replaceAll("^\\s*@prefix\\s*","").split("\\s*:\\s+")
     l1(0) -> org.semanticweb.owlapi.model.IRI.create(l1(1).replaceAll(">\\s*\\.\\s*","").replaceAll("[<>]",""))
  }).toMap

  lazy val prefixNames = prefixMap.keySet

  lazy val prefixIRIS = prefixMap.values.toList.sortBy(x => -1 * x.toString.length)

  def friendlyName(prefixMap: Map[String,org.semanticweb.owlapi.model.IRI] = prefixMap): String => String =
  {
    val prefixNames = prefixMap.keySet
    val prefixIRIS = prefixMap.values.toList.sortBy(x => -1 * x.toString.length)

    println(prefixIRIS)
    val reverse = prefixMap.map(_.swap)

    s => {
      val s1 = s.replaceAll("[<>]","")

      val bestMatch = prefixIRIS.find(p => s1.startsWith(p.toString.replaceAll("[<>]","")))
      if (bestMatch.isDefined)
        s1.replace(bestMatch.get.toString, reverse(bestMatch.get) + ":")
      else
        s1
    }
  }

  val fn = friendlyName(this.prefixMap)
  def friendlyName(s: String): String =  fn(s)


  def main(args: Array[String]): Unit = {
    prefixMap.foreach(println)
    println(prefixIRIS)
  }
}


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

  val isA = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"

  def isObjectProperty(st: Statement) = st.getObject.isInstanceOf[org.openrdf.model.Resource]
  def isDataProperty(st: Statement) = st.getObject.isInstanceOf[org.openrdf.model.Literal]
  def isIsA(st:Statement) = st.getPredicate.toString.equals(isA)


}

import rdf.readRDF._
