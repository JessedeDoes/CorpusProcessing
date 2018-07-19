package db2rdf

import org.openrdf.model.{Statement,Resource}
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

  val thing:Resource = parseStringToStatements("<http://www.w3.org/1999/02/22-rdf-syntax-ns#Resource> <http://is> <http://gek> . ")(0).getSubject

  def guess(s: Stream[org.openrdf.model.Statement]) = {
    val isAs = s.filter(isIsA)

    val classMemberships:Map[Resource, Set[Resource]] = isAs.groupBy(_.getSubject).mapValues(l => l.map(_.getObject.asInstanceOf[Resource]).toSet)

    val classes = isAs.map(_.getObject).toSet

    val objectProperties = s.filter(st => isObjectProperty(st) && !isIsA(st))


    val domainsAndRanges = objectProperties.flatMap(
      o =>
        {
          val p = o.getPredicate
          val domains = classMemberships.getOrElse(o.getSubject, Set[Resource](thing))
          val ranges = classMemberships.getOrElse(o.getObject.asInstanceOf[Resource], Set[Resource](thing))
          val z = domains.flatMap(d => ranges.map(r => (d,p,r)))
          z
        }
    ).toSet

    classes.foreach(println)
    domainsAndRanges.groupBy(_._2).foreach(println)
  }

  def main(args: Array[String]): Unit = {
    val s = parseToStatements(args(0))
    guess(s)
  }
}
