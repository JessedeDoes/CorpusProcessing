package db2rdf

object commonDefinitions {
  implicit val schema = new Schema("data/Diamant/diamant.fss")
  implicit val sort = Sort.DataPropertyType

  val INTBaseURI = "http://rdf.ivdnt.org/"


  // prefixes

  val owlPrefix = "http://www.w3.org/2002/07/owl#"
  val lemonPrefix = "http://lemon-model.net/lemon#"
  val ontolexPrefix = "http://www.w3.org/ns/lemon/ontolex#" // lemon of ontolex ????

  val diamantSchemaPrefix: String = INTBaseURI + "schema/diamant#"
  val rdfPrefix = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
  val rdfsPrefix = "http://www.w3.org/2000/01/rdf-schema#"
  val wnPrefix = "http://wordnet-rdf.princeton.edu/ontology#"
  val isocatPrefix = "http://www.isocat.org/ns/dcr.rdf#"
  val skosPrefix = "http://www.w3.org/2004/02/skos/core#"
  val lexinfoPrefix = "http://www.lexinfo.net/ontology/2.0/lexinfo#"
  val provPrefix = "http://www.w3.org/ns/prov#"
  val nifPrefix = "http://persistence.uni-leipzig.org/nlp2rdf/ontologies/nif-core#"
  val dcTermsPrefix = "http://dublincore.org/2012/06/14/dcterms.ttl#"

  val udPrefix = "http://universaldependencies.org/u/"
  val udFeatPrefix = "http://universaldependencies.org/u/feat/"

  // properties


  def objectProperty(s:String) = IRI(s, schema) (Sort.ObjectPropertyType)
  def dataProperty(s:String) = IRI(s, schema) (Sort.DataPropertyType)
  def owlClass(s:String) = IRI(s, schema) (Sort.ClassType)

  val writtenRep:IRI = dataProperty(s"${ontolexPrefix}writtenRep")
  val lexicalForm:IRI = objectProperty(s"${ontolexPrefix}lexicalForm")
  val canonicalForm:IRI = objectProperty(s"${ontolexPrefix}canonicalForm")
  val hyphenation:IRI = dataProperty(s"${diamantSchemaPrefix}hyphenation")
  val attestation:IRI = objectProperty(s"${diamantSchemaPrefix}attestation")

  val text = objectProperty(s"${diamantSchemaPrefix}text")
  val pos = objectProperty(s"${udPrefix}pos")


  val beginIndex = dataProperty(s"${nifPrefix}beginIndex")
  val endIndex = dataProperty(s"${nifPrefix}endIndex")

  val subsense = objectProperty(s"${diamantSchemaPrefix}subsense")
  val reference = objectProperty(s"${ontolexPrefix}reference")
  val senseDefinition = objectProperty(s"${lemonPrefix}definition")
  val definitionText = dataProperty(s"${diamantSchemaPrefix}definitionText")


  val evokes = objectProperty(s"${ontolexPrefix}evokes")



  val rdfsType = IRI(s"${rdfsPrefix}type")

  val prefLabel = dataProperty(s"${skosPrefix}prefLabel")
  val altLabel = dataProperty(s"${skosPrefix}altLabel")
  val skosBroader = objectProperty(s"${skosPrefix}broader")
  val skosNarrower = objectProperty(s"${skosPrefix}narrower")
  val skosRelated = objectProperty(s"${skosPrefix}related")
  val skosCloseMatch = objectProperty(s"${skosPrefix}closeMatch")
  val yearFrom = dataProperty(s"${diamantSchemaPrefix}witnessYearFrom")
  val yearTo = dataProperty(s"${diamantSchemaPrefix}witnessYearTo")
  val dcTitle = dataProperty(s"${dcTermsPrefix}title")
  val dcAuthor = dataProperty(s"${dcTermsPrefix}creator")


  val isA = rdfsType

  // classes
  val conceptType = owlClass(s"${skosPrefix}Concept")
  val lexicalConceptType = owlClass(s"${ontolexPrefix}LexicalConcept")
  val formType = owlClass(s"${ontolexPrefix}Form")
  val synonymDefinitionType = owlClass(s"${diamantSchemaPrefix}SynonymDefinition")
  val semanticRelationType  = owlClass(s"${diamantSchemaPrefix}SemanticRelation")
  // resource prefixes

  val diamantBaseURI: String = INTBaseURI + "lexica/diamant/"
  val quotationResourcePrefix: String = diamantBaseURI + "quotation/"
  val senseResourcePrefix: String = diamantBaseURI + "sense/"
  val definitionResourcePrefix: String = diamantBaseURI + "definition/"
  val synonymDefinitionResourcePrefix: String = diamantBaseURI + "synonymdefinition/"
  val entryResourcePrefix: String = diamantBaseURI + "entry/"
  val canonicalFormResourcePrefix = diamantBaseURI + "canonicalform/"
  val attestationResourcePrefix: String = diamantBaseURI + "attestation/"
  val synsetResourcePrefix: String = diamantBaseURI + "synset/"
  val wordformResourcePrefix: String = diamantBaseURI + "wordform/"
  val conceptResourcePrefix: String = diamantBaseURI + "concept/"
  val similarityResourcePrefix: String = diamantBaseURI + "similarity/"
  val semanticRelationResourcePrefix: String = diamantSchemaPrefix + "relation/" // relaties moeten in het schema komen

}
