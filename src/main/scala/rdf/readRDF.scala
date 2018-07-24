package rdf

import java.io.{FileInputStream, StringReader}
import java.util.zip.GZIPInputStream

import guru.nidi.graphviz.engine.{Format, Graphviz}
import guru.nidi.graphviz.parse._
import org.openrdf.model.{Graph, Resource, Statement}
import org.openrdf.rio.{RDFFormat, Rio}
import org.postgresql.util.ReaderInputStream
import utils.PostProcessXML

import scala.collection.JavaConverters._
import scala.util.Try
import scala.xml._

object Settings
{
  val prefixes =  s"""
                    |@prefix dc: <http://purl.org/dc/elements/1.1/#> .
                    |@prefix dcterms: <http://dublincore.org/2012/06/14/dcterms.ttl#> .
                    |@prefix owl: <http://www.w3.org/2002/07/owl#> .
                    |@prefix pwn: <http://wordnet-rdf.princeton.edu/ontology#> .
                    |@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
                    |@prefix xml: <http://www.w3.org/XML/1998/namespace> .
                    |@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
                    |@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
                    |@prefix quest: <http://obda.org/quest#> .
                    |@prefix skos: <http://www.w3.org/2004/02/skos/core#> .
                    |@prefix diamant: <http://rdf.ivdnt.org/schema/diamant#> .
                    |@prefix lemon: <http://lemon-model.net/lemon#> .
                    |@prefix lexinfo: <http://www.lexinfo.net/ontology/2.0/lexinfo#> .
                    |@prefix ontolex: <http://www.w3.org/ns/lemon/ontolex#> .
                    |@prefix nif: <http://persistence.uni-leipzig.org/nlp2rdf/ontologies/nif-core#> .
                    |@prefix olia_top: <http://purl.org/olia/olia-top.owl> .
                    |@prefix prov: <http://www.w3.org/ns/prov#> .
                    |@prefix ud: <http://universaldependencies.org/u/> .
                    |@prefix int: <http://rdf.ivdnt.org/schema/anw/> .
                    |@prefix vartrans: <http://www.w3.org/ns/lemon/vartrans#> .
                    |@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
                    |@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
                    |@prefix synsem: <http://www.w3.org/ns/lemon/synsem#> .
                    |@prefix lexinfo: <http://www.lexinfo.net/ontology/2.0/lexinfo> .
                    |@prefix dcterms: <http://purl.org/dc/terms/> .
                    |@prefix fabio: <http://purl.org/spar/fabio/> .
                    |@prefix wordnet: <http://wordnet-rdf.princeton.edu/wn31/> .
                    |@prefix lmf: <http://www.lexinfo.net/lmf> .
                    |@prefix intskos: <http://ws-boelhouwer:8080/fuseki/intskos/> .
                    |@prefix celex: <http://ws-boelhouwer:8080/fuseki/celex/> .
                    |@prefix xs: <http://www.w3.org/2001/XMLSchema#> .
                    |@prefix prov: <http://www.w3.org/ns/prov#> .
                    |@prefix olia: <http://www.w3.org/ns/olia#> .
                    |@prefix olio: <http://www.w3.org/ns/olia#> .
                    |@prefix gold: <http://www.w3.org/ns/gold#> .
                    |@prefix ontolex: <http://www.w3.org/ns/lemon/ontolex#> .
                    |            @prefix synsem: <http://www.w3.org/ns/lemon/synsem#> .

                    |            @prefix dcterms: <http://purl.org/dc/terms/> .

                    |            @prefix madsrdf: <http://www.loc.gov/mads/rdf/v1#> .
                    |            @prefix wordnet: <http://wordnet-rdf.princeton.edu/wn31/> .
                    |            @prefix ontolex: <http://www.w3.org/ns/lemon/ontolex> .
                    |            @prefix decomp: <http://www.w3.org/ns/lemon/decomp#> .

                    |            @prefix skos: <http://www.w3.org/2004/02/skos/core#> .
                    |            @prefix intskos: <http://ws-boelhouwer:8080/fuseki/intskos/> .

                    |@prefix : <#> .\n""".stripMargin
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
    if (s.isEmpty) r.stringValue().replaceAll("^[A-Za-z]","") else s
  }
  def shortName(r:  org.openrdf.model.URI) = {
    val s = r.stringValue().replaceAll("[#/]$","").replaceAll(".*(#|/)", "")
    if (s.isEmpty) r.stringValue().replaceAll("^[A-Za-z]","") else s
  }

  val isA = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"

  def isObjectProperty(st: Statement) = st.getObject.isInstanceOf[org.openrdf.model.Resource]
  def isDataProperty(st: Statement) = st.getObject.isInstanceOf[org.openrdf.model.Literal]
  def isIsA(st:Statement) = st.getPredicate.toString.equals(isA)


}

import rdf.readRDF._
