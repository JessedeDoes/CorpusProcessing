package posmapping

import db2rdf.Schema
import scala.collection.JavaConverters._

object ontologyFromTagset {

  def ontologize(tagset: TagSet) =
  {
    val prefix = "http://rdf.ivdnt.org/schema/tagsets/ud#"

    def capFirst(s: String) = s.head.toUpper + s.tail

    def hasFeature(f: String) = s"${prefix}has${capFirst(f)}"

    val ontology = Schema.newOntology()

    val word = ontology.createClass(s"${prefix}MSD")

    val posClasses = tagset.posTags.map(c =>
      {
        val c1 =
        ontology.createClass(s"$prefix$c")
        ontology.addSubclassAxiom(c1, word)
      })

    val featureClasses = tagset.partitions.map(
      { case (n,vs) => vs.map(v =>
        ontology.createClass(s"$prefix$n/$v"))}
    )

    val partitions = tagset.partitions.map(
      {
        case (n,vs) =>
          val c1 = ontology.createClass(hasFeature(n))
          vs.map(
            v => {
              val c2 = ontology.createClass(s"$prefix$n.$v")
              ontology.addSubclassAxiom(c2, c1)
            }
          )
          //vs.map(v => ontology.createClass(s"$prefix$n/$v"))
      }
    )

    val f2p = tagset.pos2partitions.toList.flatMap({case (k,v) => v.map(v1 => k -> v1)}).groupBy(_._2).mapValues(l => l.map(_._1))


    Console.err.println(f2p)
    f2p.map(
      {
        case (f,ps) =>
          Console.err.println(f)
          val c1 = ontology.createClass(hasFeature(f))
          val pclasses = ps.map(p =>  ontology.createClass(s"$prefix$p")).toSet
          Console.err.println(pclasses)
          val union:String = ontology.addUnionOfClasses(pclasses)
          Console.err.println(union)
          val uClass = ontology.createClass(union)
          ontology.addSubclassAxiom(c1, uClass)
          ontology.addSubclassAxiom(uClass, word)
      }
    )

    ontology.saveToFile("/tmp/test.fss")
    Schema.fromFile("/tmp/test.fss").createImage
  }

  def main(args: Array[String]): Unit = {
    ontologize(UDTagSet)
  }
}
