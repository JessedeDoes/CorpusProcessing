package db2rdf

import org.openrdf.model.{Statement,Resource,Value,Literal}
import org.openrdf.rio.helpers.StatementCollector
import org.openrdf.rio.{RDFFormat, Rio}
import org.semanticweb.owlapi.model._

import propositional._


import scala.collection.JavaConverters._
import scala.util.Success
import org.openrdf.model.Statement
import org.openrdf.model.URI
import utils.readRDF._

object guessSchema {

  val duelme = "/mnt/Projecten/CLARIAH/WP3_Linguistics/Search/LLOD/Data/DUELME/rdf/duelme.ttl"
  val celex = "/mnt/Projecten/CLARIAH/WP3_Linguistics/Search/LLOD/Data/CELEX/rdf/celex.ttl"

  val thing:Resource = parseStringToStatements("<http://www.w3.org/1999/02/22-rdf-syntax-ns#Resource> <http://is> <http://gek> . ")(0).getSubject

  def guess(s: Stream[org.openrdf.model.Statement]) = {
    val isAs = s.filter(isIsA)

    val classMemberships:Map[Resource, Set[Resource]] = isAs.groupBy(_.getSubject).mapValues(l => l.map(_.getObject.asInstanceOf[Resource]).toSet)

    val classes = isAs.map(_.getObject).toSet

    val objectProperties = s.filter(st => isObjectProperty(st) && !isIsA(st))
    val dataProperties = s.filter(isDataProperty)


    // TODO to do this properly, work with Unions of the found domains and ranges

    val objectPropertyDomainsAndRanges = objectProperties.flatMap(
      o =>
        {
          val p = o.getPredicate
          val domains = classMemberships.getOrElse(o.getSubject, Set[Resource](thing))
          val ranges = classMemberships.getOrElse(o.getObject.asInstanceOf[Resource], Set[Resource](thing))
          val z = domains.flatMap(d => ranges.map(r => (d,p,r)))
          z
        }
    ).toSet

    val dataPropertyDomainsAndRanges = dataProperties.flatMap(
      o =>
      {
        val p = o.getPredicate
        val domains = classMemberships.getOrElse(o.getSubject, Set[Resource](thing))
        val objct:org.openrdf.model.Literal = o.getObject.asInstanceOf[org.openrdf.model.Literal]

        val rangeDatatype = if (objct.getDatatype != null) objct.getDatatype.toString else Schema.String
        //val ranges = classMemberships.getOrElse(o.getObject.asInstanceOf[Resource], Set[Resource](thing))
        val z = domains.map(d => (d,p,rangeDatatype))
        z
      }
    ).toSet

    // classes.foreach(println)

    objectPropertyDomainsAndRanges.groupBy(_._2).foreach(println)

    val o = Schema.newOntology()

    objectPropertyDomainsAndRanges.foreach(
      {case (d,p,r) => o.addObjectProperty(d.toString,p.toString,r.toString)}
    )

    dataPropertyDomainsAndRanges.foreach(
      {case (d,p,r) => o.addDataProperty(d.toString,p.toString,r.toString)}
    )

    o.saveToFile("/tmp/test.fss")

    val schema = Schema(new java.io.FileInputStream("/tmp/test.fss"))
    schema.createImage
  }

  def main(args: Array[String]): Unit = {
    val s = parseToStatements(args(0))
    guess(s)
  }
}
