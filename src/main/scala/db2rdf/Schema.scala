package db2rdf

import org.semanticweb.owlapi.model.OWLClass

import scala.collection.JavaConverters._

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

  //axioms.foreach(println)

  val objectPropertyNames = objectProperties.map(op => op.getIRI.toString)
  val dataPropertyNames = dataProperties.map(op => op.getIRI.toString)
  val classNames = classes.map(op => op.getIRI.toString)

  def validObjectProperty(s: String):Boolean = objectPropertyNames.contains(s)
  def validDataProperty(s: String):Boolean = dataPropertyNames.contains(s)
  def validClass(s: String):Boolean = classNames.contains(s)


}

object testSchema
{
  val s = new Schema("data/Diamant/diamant.fss")

  def main(args: Array[String]): Unit =
  {
    s.classNames.toList.sortBy(identity).foreach(println)
    s.objectPropertyNames.toList.sortBy(identity).foreach(println)
  }
}
