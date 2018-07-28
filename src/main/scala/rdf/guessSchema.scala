package rdf

import org.openrdf.model.Resource
import readRDF._

object guessSchema {

  val duelme = "/mnt/Projecten/CLARIAH/WP3_Linguistics/Search/LLOD/Data/DUELME/rdf/duelme.ttl"
  val celex = "/mnt/Projecten/CLARIAH/WP3_Linguistics/Search/LLOD/Data/CELEX/rdf/celex.ttl"

  val thing:Resource = parseStringToStatements("<http://www.w3.org/1999/02/22-rdf-syntax-ns#Resource> <http://is> <http://gek> . ")(0).getSubject

  def sample[T](s: Stream[T], n: Int) =
    s.zipWithIndex.filter({case (t,i) => i % n == 0}).map(_._1)

  def createUnion(o: OntologyManagement, s: Set[Resource]) =
    if (s.size == 1) s.head.toString else
      {
        val classes = s.map(x => o.createClass(x.toString))
        val union = o.addUnionOfClasses(classes)
        union
      }

  def guess(s0: Stream[org.openrdf.model.Statement], sampleRate: Option[Int] = None) = {

    val s = if (sampleRate.isDefined) sample(s0,sampleRate.get) else s0

    Console.err.println(s.size)

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

    //objectPropertyDomainsAndRanges.groupBy(_._2).foreach(println)
    val opGrouped = objectPropertyDomainsAndRanges.groupBy(_._2).mapValues(s => (s.map(_._1), s.map(_._3)))

    val o = Schema.newOntology()

    opGrouped.foreach(
      {
        case (p, (d,r)) =>
          val domain = createUnion(o,d)
          val range = createUnion(o,r)
          o.addObjectProperty(domain, p.toString, range)
      }
    )


    //objectPropertyDomainsAndRanges.foreach(
    //  {case (d,p,r) => o.addObjectProperty(d.toString,p.toString,r.toString)}
    // )

    dataPropertyDomainsAndRanges.foreach(
      {case (d,p,r) => o.addDataProperty(d.toString,p.toString,r.toString)}
    )

    o.saveToFile("/tmp/test.fss")

    val schema = Schema(new java.io.FileInputStream("/tmp/test.fss"))
    schema.createImage
    schema.readableVersion()
  }

  def main(args: Array[String]): Unit = {
    val s = parseToStatements(args(0))
    guess(s, if (args.size > 1) Some(args(1).toInt) else None)
  }
}
