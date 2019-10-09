package rdf

import org.openrdf.model.Statement

object Settings
{
  val prefixes =  s"""
                     |@prefix celex: <http://ws-boelhouwer:8080/fuseki/celex/> .
                     |@prefix dc: <http://purl.org/dc/elements/1.1/#> .
                     |@prefix dcterms: <http://dublincore.org/2012/06/14/dcterms.ttl#> .
                     |@prefix dcterms: <http://purl.org/dc/terms/> .
                     |@prefix decomp: <http://www.w3.org/ns/lemon/decomp#> .
                     |@prefix diamant: <http://rdf.ivdnt.org/schema/diamant#> .
                     |@prefix int: <http://rdf.ivdnt.org/schema/int#> .
                     |@prefix anw: <http://rdf.ivdnt.org/schema/anw#> .
                     |@prefix fabio: <http://purl.org/spar/fabio/> .
                     |@prefix gold: <http://www.w3.org/ns/gold#> .
                     |@prefix int: <http://rdf.ivdnt.org/schema/anw/> .
                     |@prefix intskos: <http://ws-boelhouwer:8080/fuseki/intskos/> .
                     |@prefix lemon: <http://lemon-model.net/lemon#> .
                     |@prefix lexinfo: <http://www.lexinfo.net/ontology/2.0/lexinfo> .
                     |@prefix lexinfo: <http://www.lexinfo.net/ontology/2.0/lexinfo#> .
                     |@prefix lmf: <http://www.lexinfo.net/lmf#> .
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
                     |@prefix frbr: <http://purl.org/vocab/frbr/core#> .
                     |@prefix cito: <http://purl.org/spar/cito/> .
                     |@prefix time: <http://www.w3.org/2006/time#> .
                     |@prefix oa: <http://www.w3.org/ns/oa#> .
                     |@prefix : <#> .\n""".stripMargin



  lazy val prefixMap:Map[String,org.semanticweb.owlapi.model.IRI] = prefixes.split("\n").filter(_.contains("@")).toStream.map(l => {
    val l1 = l.replaceAll("^\\s*@prefix\\s*","").split("\\s*:\\s+")
    println(l1.toList)
    l1(0) -> org.semanticweb.owlapi.model.IRI.create(l1(1).replaceAll(">\\s*\\.\\s*","").replaceAll("[<>]",""))
  }).toMap

  lazy val prefixNames = prefixMap.keySet

  lazy val prefixIRIS = prefixMap.values.toList.sortBy(x => -1 * x.toString.length)

  def friendlyName(prefixMap: Map[String,org.semanticweb.owlapi.model.IRI] = prefixMap): String => String =
  {
    val prefixNames = prefixMap.keySet
    val prefixIRIS = prefixMap.values.toList.sortBy(x => -1 * x.toString.length)


    val reverse = prefixMap.map(_.swap)

    s => {
      val s1 = s.replaceAll("[<>]","")

      val bestMatch = prefixIRIS.find(p => s1.startsWith(p.toString.replaceAll("[<>]","")))
      val s2 = if (bestMatch.isDefined)
        s1.replace(bestMatch.get.toString, reverse(bestMatch.get) + ":").replaceAll("^:", "")
      else
        s1
      Console.err.println("Friendly: " + s2)
      s2
    }
  }

  val fn = friendlyName(this.prefixMap)
  def friendlyName(s: String): String =  fn(s)

  val isA = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" // http://www.w3.org/2000/01/rdf-schema#type
  val isA2 = "http://www.w3.org/2000/01/rdf-schema#type"  // deze zit nu in diamant maar is fout?

  val isaas = List(isA,isA2)

  def isIsA(st:Statement) =
    isaas.exists(x => st.getPredicate.toString.replaceAll("[<>]","").equals(x.replaceAll("[<>]","")))
  def main(args: Array[String]): Unit = {
    prefixMap.foreach(println)
    println(prefixIRIS)
  }
}
