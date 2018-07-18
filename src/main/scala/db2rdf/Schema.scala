package db2rdf

import org.openrdf.model.Statement
import org.openrdf.rio.helpers.StatementCollector
import org.openrdf.rio.{RDFFormat, Rio}
import org.semanticweb.owlapi.model._
import propositional._
import utils.readRDF.rdfParser

import scala.collection.JavaConverters._
import scala.util.Success
import org.openrdf.model.Statement

class Schema(fileName: String) {

  import org.semanticweb.owlapi.apibinding.OWLManager
  import org.semanticweb.owlapi.model.OWLOntology
  import org.semanticweb.owlapi.model.OWLOntologyManager

  val manager: OWLOntologyManager = OWLManager.createOWLOntologyManager
  val ontology: OWLOntology = manager.loadOntologyFromOntologyDocument(new java.io.File(fileName))

  val classes:Set[OWLClass] = ontology.getClassesInSignature().asScala.toSet

  val objectProperties = ontology.getObjectPropertiesInSignature().asScala.toSet
  val dataProperties = ontology.getDataPropertiesInSignature().asScala.toSet
  val axioms = ontology.getTBoxAxioms(null).asScala.toSet

  val parser = Rio.createParser(RDFFormat.NTRIPLES)

  //axioms.foreach(println)

  val objectPropertyNames = objectProperties.map(op => op.getIRI.toString)


  def parse(s: String):org.openrdf.model.Statement = {
    val in = new java.io.StringReader(s)

    val myGraph = new org.openrdf.model.impl.GraphImpl

    val collector = new StatementCollector(myGraph)

    parser.setRDFHandler(collector)

    parser.parse(in, "http://")

    myGraph.iterator().next()
  }

  def classDeclarations = classes.map(c => {
     val name = c.toString
     val stmt = s"$name <${utils.readRDF.isA}> <http://Class> ."
      Console.err.println(stmt)
     scala.util.Try(parse(stmt))
  }).toSeq.filter(_.isSuccess).map(_.asInstanceOf[Success[org.openrdf.model.Statement]]).map(_.value.asInstanceOf[org.openrdf.model.Statement])

  classDeclarations.foreach(println)
  def subClassDefinitions:Seq[org.openrdf.model.Statement] =
  {
    val subClassAxioms = axioms.filter(a => a.getAxiomType==AxiomType.SUBCLASS_OF).map(_.asInstanceOf[OWLSubClassOfAxiom])

    val subClassDefinitions =subClassAxioms.map(
      a =>
        {
          val sub = a.getSubClass.toString
          val p = "<http://subClassOf>"
          val supy = a.getSuperClass.toString
          val stmt = s"$sub $p $supy ."
          println(stmt)
          scala.util.Try(parse(stmt))
        }
    )
    subClassDefinitions.toList.filter(_.isSuccess).map(_.asInstanceOf[Success[org.openrdf.model.Statement]]).map(_.value.asInstanceOf[org.openrdf.model.Statement]).toSeq
  }

  def dataPropertyDefinitions:Seq[org.openrdf.model.Statement] = {


    val dataPropertyDomainAxioms = axioms.filter(a => a.getAxiomType == AxiomType.DATA_PROPERTY_DOMAIN).map(_.asInstanceOf[OWLDataPropertyDomainAxiom]).toList
    val dataPropertyRangeAxioms = axioms.filter(a => a.getAxiomType == AxiomType.DATA_PROPERTY_RANGE).map(_.asInstanceOf[OWLDataPropertyRangeAxiom]).toList

    val types = axioms.groupBy(_.getAxiomType).mapValues(x => x.size)

    //println(types)

    val rangeMap = dataPropertyRangeAxioms.map(a => {
      val prop = a.getDataPropertiesInSignature.iterator().next()
      val range = a.getRange
      prop.toString -> range.toString
    }).toMap

    val dataPropertyDefinitions: List[org.openrdf.model.Statement] = dataPropertyDomainAxioms.map(a => {
      val prop = a.getDataPropertiesInSignature.iterator().next().toString
      val domain = a.getDomain.toString

      val range = rangeMap.getOrElse(prop.toString, "http://whaddever")

      val r1 = s""""${if (range.contains("<")) range.replaceAll("[<>]","") else range}""""

      val d1 = if (domain.contains("<")) domain else s"<$domain>"
      val p1 = if (prop.contains("<")) prop else s"<$prop>"

      val stmt = s"${d1} ${p1} ${r1} ."

      parse(stmt)

    })
    dataPropertyDefinitions.toSeq
  }


  def objectPropertyDefinitions:Seq[org.openrdf.model.Statement] = {

    val objectPropertyDomainAxioms = axioms.filter(a => a.getAxiomType == AxiomType.OBJECT_PROPERTY_DOMAIN).map(_.asInstanceOf[OWLObjectPropertyDomainAxiom]).toList
    val obectPropertyRangeAxioms = axioms.filter(a => a.getAxiomType == AxiomType.OBJECT_PROPERTY_RANGE).map(_.asInstanceOf[OWLObjectPropertyRangeAxiom]).toList

    val types = axioms.groupBy(_.getAxiomType).mapValues(x => x.size)

    //println(types)

    val rangeMap = obectPropertyRangeAxioms.map(a => {
      val prop = a.getObjectPropertiesInSignature.iterator().next()
      val range = a.getRange
      prop.toString -> range.toString
    }).toMap

    val objectPropertyDefinitions: List[org.openrdf.model.Statement] = objectPropertyDomainAxioms.map(a => {
      val prop = a.getObjectPropertiesInSignature.iterator().next()
      val domain = a.getDomain
      val range = rangeMap(prop.toString)
      val r1 = if (range.contains("<")) range else s"<$range>"
      val stmt = s"${domain} ${prop} ${r1} ."
      //println(stmt)
      parse(stmt)
    })
    objectPropertyDefinitions
  }

  (classDeclarations ++ subClassDefinitions ++ objectPropertyDefinitions ++ dataPropertyDefinitions).foreach(println)

  lazy val dot = utils.readRDF.makeDot(classDeclarations ++ subClassDefinitions ++ objectPropertyDefinitions ++ dataPropertyDefinitions)

  Console.err.println(dot)

  val dataPropertyNames = dataProperties.map(op => op.getIRI.toString)
  val classNames = classes.map(op => op.getIRI.toString)

  def validObjectProperty(s: String):Boolean = objectPropertyNames.contains(s)
  def validDataProperty(s: String):Boolean = dataPropertyNames.contains(s)
  def validClass(s: String):Boolean = classNames.contains(s)

  def prop2owl(p: Proposition): String =
  {
    p match {
      case Literal(a) => a
      case Not(a) => s"ObjectComplementOf(${prop2owl(a)})"
      case And(a, b) => s"ObjectIntersectionOf((${prop2owl(a)}) (${prop2owl(b)}) )"
      case Or(a, b) => s"ObjectUnionOf(${prop2owl(a)} ${prop2owl(b)} )"
      case Implies(a, b) => s"SubclassOf((${prop2owl(a)}) (${prop2owl(b)}) )"
      case Equiv(a, b) => s"EquivalentClasses(${prop2owl(a)} ${prop2owl(b)} )"
    }
  }
}

object testSchema
{
  val olia = "data/olia/olia.nt"
  val diamant = "data/Diamant/diamant.fss"
  val s = new Schema(diamant)

  def main(args: Array[String]): Unit =
  {
    System.exit(0)
    println("#classes:")
    s.classNames.toList.sortBy(identity).foreach(println)
    println("#object properties")
    s.objectPropertyNames.toList.sortBy(identity).foreach(println)
    println("#data properties")
    s.dataPropertyNames.toList.sortBy(identity).foreach(println)
    println("#axioms")
    s.axioms.foreach(a => println(s"$a ${a.getAxiomType}"))
  }
}
