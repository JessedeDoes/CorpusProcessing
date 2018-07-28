package rdf

import java.io.FileWriter

import org.openrdf.rio.helpers.StatementCollector
import org.openrdf.rio.{RDFFormat, Rio}
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.formats.FunctionalSyntaxDocumentFormat
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.model.parameters.Imports
import propositional._
import uk.ac.manchester.cs.owl.owlapi.OWLDataFactoryImpl

import scala.collection.JavaConverters._
import scala.util.Success

object Schema
{
  def fromFile(fileName: String) = Schema(new java.io.FileInputStream(fileName))
  def fromURL(url: String) = {
    val u = new java.net.URL(url)
    Schema(u.openStream())
  }

  def newOntology(): OntologyManagement =
  {
    val manager: OWLOntologyManager = OWLManager.createOWLOntologyManager
    val o = manager.createOntology()
    OntologyManagement(o,manager)
  }

  val String = "http://www.w3.org/2001/XMLSchema#string"
  val Integer = "http://www.w3.org/2001/XMLSchema#integer"

  def test() =
  {
    val om = newOntology()

    om.addDataProperty("http://kwiep", "http://kwap", Schema.String)
    om.addObjectProperty("http://kwiep", "http://kwap", "http://kwup")
    om.saveToFile("/tmp/test.fss")
  }

  def main(args: Array[String]): Unit = {
    test()
  }
}


import org.semanticweb.owlapi.model.{AddAxiom, OWLAxiom, OWLClass}


case class OntologyManagement(ontology: OWLOntology, manager: OWLOntologyManager )
{

  def createIRI(s: String) = org.semanticweb.owlapi.model.IRI.create(s)
  val isA = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"

  val df: OWLDataFactory = new OWLDataFactoryImpl()


  def addSubclassAxiom(clsA: OWLClass, clsB: OWLClass) =
  {

    //val clsA = df.getOWLClass(createIRI(pizza_iri + "#Calzone"))
    //val clsB = df.getOWLClass(createIRI(pizza_iri + "#Pizza"))

    // Now create the axiom

    val axiom = df.getOWLSubClassOfAxiom(clsA, clsB)
    // add the axiom to the ontology.
    val addAxiom = new AddAxiom(ontology, axiom)
    // We now use the manager to apply the change
    manager.applyChange(addAxiom)
    // remove the axiom from th
  }

  def createClass(iri: String):OWLClass =
  {
    val cls = df.getOWLClass(createIRI(iri))
    cls
    //val axiom = df.getOWLCl
  }


  def addObjectProperty(domain: String, propName: String, range: String): Unit =
  {
    Console.err.println(s"ADD: $domain $propName $range")
    val domainClass = createClass(domain)
    val rangeClass = createClass(range)
    val piri = createIRI(propName)
    val property = df.getOWLObjectProperty(piri)

    val axiom = df.getOWLObjectPropertyDomainAxiom(property, domainClass)

    val addAxiom = new AddAxiom(ontology, axiom)
    // We now use the manager to apply the change
    manager.applyChange(addAxiom)

    val axiom1 = df.getOWLObjectPropertyRangeAxiom(property, rangeClass)

    val addAxiom1 = new AddAxiom(ontology, axiom1)
    // We now use the manager to apply the change
    manager.applyChange(addAxiom1)
  }

  def addUnionOfClasses(disjuncts: Set[OWLClass]): String =
  {
    val union:OWLObjectUnionOf = df.getOWLObjectUnionOf(disjuncts.asJava) // nee veel te simpel

    val newName =  (disjuncts.head.toString.replaceAll(">","")  ::
      disjuncts.tail.map(_.toString.replaceAll(".*[/#:]","")).toList).mkString("∪").replaceAll("[<>]","")

    val newClass = createClass(newName)

    disjuncts.foreach(addSubclassAxiom(_, newClass)) // dit doe je eigenlijk te laat hier

    val equivAxiom = df.getOWLEquivalentClassesAxiom(newClass, union)

    val addAxiom = new AddAxiom(ontology, equivAxiom)
    manager.applyChange(addAxiom)

    newName
  }

  def addDataProperty(domain: String, propName: String, range: String): Unit =
  {
    val domainClass = createClass(domain)
    val rangeIRI = createIRI(range)
    val rangeType = df.getOWLDatatype(rangeIRI)

    val piri = createIRI(propName)

    val property = df.getOWLDataProperty(piri)


    val axiom = df.getOWLDataPropertyDomainAxiom(property, domainClass)

    val addAxiom = new AddAxiom(ontology, axiom)
    // We now use the manager to apply the change
    manager.applyChange(addAxiom)

    val axiom1 = df.getOWLDataPropertyRangeAxiom(property, rangeType)

    val addAxiom1 = new AddAxiom(ontology, axiom1)
    // We now use the manager to apply the change
    manager.applyChange(addAxiom1)
  }


  def initResource() = addObjectProperty("http://www.w3.org/2000/01/rdf-schema#Resource", isA , "http://Class")

  def saveToFile(fileName: String) = {
    import org.semanticweb.owlapi.model.IRI
    val formatTo = new  FunctionalSyntaxDocumentFormat

    manager.saveOntology(ontology, formatTo, IRI.create(new java.io.File(fileName).toURI))
  }
}

case class Schema(inputStream: java.io.InputStream) {

  import org.semanticweb.owlapi.apibinding.OWLManager
  import org.semanticweb.owlapi.model.{OWLOntology, OWLOntologyManager}

  // http://owlcs.github.io/owlapi/apidocs_5/uk/ac/manchester/cs/owl/owlapi/OWLDataFactoryImpl.html

  //df.getOWLClass()

  val manager: OWLOntologyManager = OWLManager.createOWLOntologyManager

  val ontology: OWLOntology = manager.loadOntologyFromOntologyDocument(inputStream)

  val management = OntologyManagement(ontology, manager)
  //management.initResource


  val cc:Set[OWLAxiom] = ontology.getTBoxAxioms(Imports.INCLUDED).asScala.toSet //.flatMap(o => o.getClassesInSignature())
  val cc1 = cc.flatMap(o => o.getClassesInSignature.asScala.toSet)
  val classes:Set[OWLClass] = cc1 ++ ontology.getClassesInSignature(Imports.INCLUDED).asScala.toSet

  val objectProperties = ontology.getObjectPropertiesInSignature().asScala.toSet
  val dataProperties = ontology.getDataPropertiesInSignature().asScala.toSet

  val axioms = ontology.getTBoxAxioms(Imports.INCLUDED).asScala.toSet

  //val bla = ontology.getNestedClassExpressions
  //bla.asScala.foreach(println)
  //System.exit(1)

  val parser = Rio.createParser(RDFFormat.NTRIPLES)

  //axioms.foreach(println)

  val objectPropertyNames = objectProperties.map(op => op.getIRI.toString)


  def parse(s: String):org.openrdf.model.Statement = {
    val in = new java.io.StringReader(s)

    val myGraph = new org.openrdf.model.impl.GraphImpl

    val collector = new StatementCollector(myGraph)

    parser.setRDFHandler(collector)

    //Console.err.println(s)
    parser.parse(in, "http://")

    myGraph.iterator().next()
  }

  lazy val  classDeclarations = classes.map(c => {
     val name = c.toString
     val n1 = if (name.contains("<")) name else s"<$name>"

     val stmt = s"$n1 <${readRDF.isA}> <http://Class> ."
     //Console.err.println(stmt)
     scala.util.Try(parse(stmt))
  }).toSeq.filter(_.isSuccess).map(_.asInstanceOf[Success[org.openrdf.model.Statement]]).map(_.value.asInstanceOf[org.openrdf.model.Statement])

  //classDeclarations.foreach(println)


  lazy val subClassDefinitions:Seq[org.openrdf.model.Statement] =
  {
    val subClassAxioms = axioms.filter(a => a.getAxiomType==AxiomType.SUBCLASS_OF).map(_.asInstanceOf[OWLSubClassOfAxiom])

    val subClassDefinitions =subClassAxioms.map(
      a =>
        {
          val sub = getNameForClass(a.getSubClass)

          val p = "<http://subClassOf>"

          val supy = getNameForClass(a.getSuperClass)

          val stmt = s"$sub $p $supy ."
         //  println(stmt)
          scala.util.Try(parse(stmt))
        }
    )
    subClassDefinitions.toList.filter(_.isSuccess).map(_.asInstanceOf[Success[org.openrdf.model.Statement]]).map(_.value.asInstanceOf[org.openrdf.model.Statement]).toSeq
  }


  def getNameForClass(c: OWLClassExpression):String  =
  {
    val dtype = c.getClassExpressionType
    val d1 = if (dtype == ClassExpressionType.OWL_CLASS)
      c.toString
    else if (dtype == ClassExpressionType.OBJECT_UNION_OF)
      {
        val z0 = c.asDisjunctSet().asScala
        if (!z0.exists(_.isAnonymous)) {
          val z = c.asDisjunctSet().asScala.filter(!_.isAnonymous).map(_.asOWLClass)
          val newName = management.addUnionOfClasses(z.toSet)
          newName
        }  else "Some_Union"
      }
    else
      {
        //Console.err.println("COMPLEX!" + c.toString)
        val c1 = c.toString.replaceAll("<http://[^<>]*#(.*?)>","$1")
          .replaceAll("(Object|Data)(Min|Max|Exact)Cardinality","$2")
          .replaceAll(" ","༐")
          .replaceAll("\\(","⟨").replaceAll("\\)","⟩").replaceAll("[^A-Za-z0-9⟨⟩#༐]","_")

        s"<http://complex#$c1>"
      }// hier zou je dus het een een ander kunnen bijmaken.......

      //"<http://x" + c.toString.replaceAll("[^A-Za-z0-9#]","") + ">"

    if (d1.contains("<")) d1 else s"<$d1>"
  }

  lazy val dataPropertyDomainAxioms:List[OWLDataPropertyDomainAxiom] = axioms.filter(a => a.getAxiomType == AxiomType.DATA_PROPERTY_DOMAIN).map(_.asInstanceOf[OWLDataPropertyDomainAxiom]).toList
  lazy val dataPropertyRangeAxioms:List[OWLDataPropertyRangeAxiom] = axioms.filter(a => a.getAxiomType == AxiomType.DATA_PROPERTY_RANGE).map(_.asInstanceOf[OWLDataPropertyRangeAxiom]).toList
  lazy val objectPropertyDomainAxioms:List[OWLObjectPropertyDomainAxiom] = axioms.filter(a => a.getAxiomType == AxiomType.OBJECT_PROPERTY_DOMAIN).map(_.asInstanceOf[OWLObjectPropertyDomainAxiom]).toList
  lazy val objectPropertyRangeAxioms:List[OWLObjectPropertyRangeAxiom] = axioms.filter(a => a.getAxiomType == AxiomType.OBJECT_PROPERTY_RANGE).map(_.asInstanceOf[OWLObjectPropertyRangeAxiom]).toList

  lazy val dataPropertyRangeMap = dataPropertyRangeAxioms.map(a => {
    val prop = a.getDataPropertiesInSignature.iterator().next()
    val range = a.getRange
    prop.toString -> range.toString
  }).toMap

  lazy val objectPropertyRangeMap = objectPropertyRangeAxioms.map(a => {
    val prop = a.getObjectPropertiesInSignature.iterator().next()
    val range = a.getRange
    prop.toString -> getNameForClass(range)
  }).toMap


  lazy val dataPropertyDefinitions:Seq[org.openrdf.model.Statement] = {



    val types = axioms.groupBy(_.getAxiomType).mapValues(x => x.size)

    //println(types)



    val dataPropertyDefinitions: List[org.openrdf.model.Statement] = dataPropertyDomainAxioms.map(a => {
      val prop = a.getDataPropertiesInSignature.iterator().next().toString
      val domain = getNameForClass(a.getDomain)

      val range = dataPropertyRangeMap.getOrElse(prop.toString, "http://whaddever")

      val r1 = s""""${if (range.contains("<")) range.replaceAll("[<>]","") else range}""""

      val p1 = if (prop.contains("<")) prop else s"<$prop>"

      val stmt = s"${domain} ${p1} ${r1} ."

      parse(stmt)

    })
    dataPropertyDefinitions.toSeq
  }

  // TODO proper treatment of unions etc..
  // maybe add a placeholder class for each union and other complex class expression

  lazy val objectPropertyDefinitions:Seq[org.openrdf.model.Statement] = {



    val types = axioms.groupBy(_.getAxiomType).mapValues(x => x.size)

    //println(types)

    val objectPropertyDefinitions: List[org.openrdf.model.Statement] = objectPropertyDomainAxioms.map(a => {
      val prop = a.getObjectPropertiesInSignature.iterator().next().toString

      val domain = getNameForClass(a.getDomain)
      val range = objectPropertyRangeMap.getOrElse(prop.toString, "<http://www.w3.org/2000/01/rdf-schema#Resource>")

      val r2 = if (range.contains("<")) range else s"<$range>"
      val p2 = if (prop.contains("<")) prop else s"<$prop>"
      val stmt = s"${domain} ${p2} ${r2} ."
      //println("WADDE:" + stmt)
      parse(stmt)
    })
    objectPropertyDefinitions
  }

  //objectPropertyDefinitions.foreach(println)
  //System.exit(0)

  // (classDeclarations ++ subClassDefinitions ++ objectPropertyDefinitions ++ dataPropertyDefinitions).foreach(println)

  lazy val dot = rdf.diagrams.makeDot(classDeclarations ++ subClassDefinitions ++ objectPropertyDefinitions ++ dataPropertyDefinitions)

  def  createImage:Unit = createImage("./test.svg")


  def createImage(fileName: String) =
    {
      val fw = new FileWriter("/tmp/test.dot")
      fw.write(dot)
      fw.close()

      rdf.diagrams.createSVG(dot, fileName)
    }

  // Console.err.println(dot)

  val dataPropertyNames = dataProperties.map(op => op.getIRI.toString)
  val classNames = classes.map(op => op.getIRI.toString)


  def friendlyString(s: String)  = Settings.friendlyName(s)

  def getReadableNameForProperty(p: OWLObjectProperty) = friendlyString(p.toString)

  def getReadableNameForDataRange(p: OWLDataRange) = friendlyString(p.toString)



  def getReadableNameForDataProperty(p: OWLDataProperty) = friendlyString(p.toString)

  def getReadableNameForClass(c: OWLClassExpression):String  = {
    val dtype = c.getClassExpressionType
    dtype match {
      case ClassExpressionType.OWL_CLASS => friendlyString(c.toString)
      case ClassExpressionType.OBJECT_UNION_OF =>
        val disjuncts = c.asDisjunctSet().asScala.map(_.asOWLClass)
        disjuncts.map(c => friendlyString(c.toString)).mkString(" ∪ ")
      case ClassExpressionType.OBJECT_INTERSECTION_OF =>
        val conjuncts = c.asConjunctSet().asScala.map(_.asOWLClass)
        conjuncts.map(c => friendlyString(c.toString)).mkString(" ∩ ")
      case ClassExpressionType.OBJECT_EXACT_CARDINALITY =>
        val x = c.asInstanceOf[OWLObjectExactCardinality]
        val i = x.getCardinality
        val p = x.getProperty
        s"∃!$i ${getReadableNameForProperty(p.asOWLObjectProperty())}"
    }
  }




  def readableVersion(friendlyString: String=>String = Settings.friendlyName) =
  {
    val classes = this.classes.map(c => getReadableNameForClass(c)).toList.sortBy(identity)

    val op = objectProperties.map(
      o =>
        {
          val da = objectPropertyDomainAxioms.find(_.getProperty.asOWLObjectProperty() == o)
          val ra = objectPropertyRangeAxioms.find(_.getProperty.asOWLObjectProperty() == o)

          val domain = da.map(a => getReadableNameForClass(a.getDomain)).getOrElse("rdfs:Resource")
          val range  = ra.map(a => getReadableNameForClass(a.getRange)).getOrElse("rdfs:Resource")
          val name = getReadableNameForProperty(o)

          s"${name} ⊆ $domain × $range "
        }
    ).toList.sortBy(identity)


    val dp = dataProperties.map(
      o =>
      {
        val da = dataPropertyDomainAxioms.find(_.getProperty.asOWLDataProperty() == o)
        val ra = dataPropertyRangeAxioms.find(_.getProperty.asOWLDataProperty() == o)

        val domain = da.map(a => getReadableNameForClass(a.getDomain)).getOrElse("rdfs:Resource")
        val range  = ra.map(a => getReadableNameForDataRange(a.getRange)).getOrElse("rdfs:Literal")
        val name = getReadableNameForDataProperty(o)

        s"${name} ⊆ $domain × $range "
      }
    ).toList.sortBy(identity)

    println("## Classes:")
    classes.foreach(println)
    println("## Object properties:")
    op.foreach(println)

    println("## Data properties:")
    dp.foreach(println)
  }


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
  lazy val metadata = Schema.fromFile("data/Diamant/metadata.fss")
  lazy val diamantSchema = Schema.fromFile(diamant)
  lazy val ontolex = Schema.fromURL("http://www.w3.org/ns/lemon/ontolex#")
  lazy val lexinfo = Schema.fromURL("http://lexinfo.net/ontology/2.0/lexinfo.owl")
  lazy val lmf = Schema.fromFile("data/Diamant/TagmaticaLMF.owl")
  lazy val ontoCleaned = Schema.fromFile("data/Diamant/ontolex.cleaned.rdf")

  def main(args: Array[String]): Unit =
  {
    val s = if (args.size > 0) Schema.fromFile(args(0)) else lmf

    s.readableVersion()


    s.createImage

    if (false) {
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
}
