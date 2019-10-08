package db2rdf

import rdf.Schema

object commonDefinitions {
  implicit val schema = Schema.fromFile("data/Diamant/diamant.fss")
  implicit val sort = Sort.DataPropertyType

  def objectProperty(s:String) = IRI(s, schema) (Sort.ObjectPropertyType)
  def dataProperty(s:String) = IRI(s, schema) (Sort.DataPropertyType)
  def owlClass(s:String) = IRI(s, schema) (Sort.ClassType)

  val INTBaseURI = "http://rdf.ivdnt.org/"

  val diamantGraphURL: String = "http://rdf.ivdnt.org/lexica/diamant/v1.5/"
  val molexGraphURL = "http://rdf.ivdnt.org/lexica/molex/v1.0/"
  // prefixes

  val owlPrefix = "http://www.w3.org/2002/07/owl#"
  val lemonPrefix = "http://lemon-model.net/lemon#"
  val ontolexPrefix = "http://www.w3.org/ns/lemon/ontolex#" // lemon of ontolex ????


  val diamantSchemaPrefix: String = INTBaseURI + "schema/diamant#"
  val lexcitSchemaPrefix: String = if (Settings.doLexCit) INTBaseURI + "schema/lexcit#" else diamantSchemaPrefix
  val rdfPrefix = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
  val rdfsPrefix = "http://www.w3.org/2000/01/rdf-schema#"
  val wnPrefix = "http://wordnet-rdf.princeton.edu/ontology#"
  val isocatPrefix = "http://www.isocat.org/ns/dcr.rdf#"
  val skosPrefix = "http://www.w3.org/2004/02/skos/core#"
  val lexinfoPrefix = "http://www.lexinfo.net/ontology/2.0/lexinfo#"
  val provPrefix = "http://www.w3.org/ns/prov#"
  val citoPrefix = "http://purl.org/spar/cito/"
  val frbrPrefix = "http://purl.org/vocab/frbr/core#"
  val nifPrefix = "http://persistence.uni-leipzig.org/nlp2rdf/ontologies/nif-core#"
  val dcTermsPrefix = "http://dublincore.org/2012/06/14/dcterms.ttl#"

  val udPrefix = "http://universaldependencies.org/u/"
  val udFeatPrefix = "http://universaldependencies.org/u/feat/"
  val oaPrefix = "http://www.w3.org/ns/oa#"
  // properties


  val rdfsLabel: IRI = dataProperty(s"${rdfsPrefix}label")


  val odwnGraphURL = "http://odwn-rdf.vu.nl/odwn13/"
  val distanceGraphURL: String = diamantSchemaPrefix + "/similarity/"

  val diamantGraph = IRI(diamantGraphURL)

  val writtenRep:IRI = dataProperty(s"${ontolexPrefix}writtenRep")
  val lexicalForm:IRI = objectProperty(s"${ontolexPrefix}lexicalForm")
  val canonicalForm:IRI = objectProperty(s"${ontolexPrefix}canonicalForm")
  val sense:IRI = objectProperty(s"${ontolexPrefix}sense")
  val embodiment:IRI = objectProperty(s"${frbrPrefix}embodiment")
  val embodimentOf:IRI = objectProperty(s"${frbrPrefix}embodimentOf")
  val canonicalHistoricalForm:IRI = objectProperty(s"${diamantSchemaPrefix}canonicalHistoricalForm")
  val canonicalModernForm:IRI = objectProperty(s"${diamantSchemaPrefix}canonicalModernForm")
  val hyphenation:IRI = dataProperty(s"${diamantSchemaPrefix}hyphenation")
  val attestation:IRI = objectProperty(s"${lexcitSchemaPrefix}attestation")
  val locus:IRI = objectProperty(s"${lexcitSchemaPrefix}locus")

  val diamantTextOfAttestation = objectProperty(s"${diamantSchemaPrefix}text")
  val hasCitedEntity = objectProperty(s"${citoPrefix}hasCitedEntity")
  val hasCitingEntity = objectProperty(s"${citoPrefix}hasCitingEntity")
  val pos = objectProperty(s"${udPrefix}pos")

  val hasBody = objectProperty(s"${oaPrefix}hasBody")
  val hasTarget = objectProperty(s"${oaPrefix}hasTarget")
  val hasSource = objectProperty(s"${oaPrefix}hasSource")
  val hasSelector = objectProperty(s"${oaPrefix}hasSelector")
  val oaStart = objectProperty(s"${oaPrefix}start")
  val oaEnd = objectProperty(s"${oaPrefix}end")



  val beginIndex = dataProperty(s"${nifPrefix}beginIndex")
  val endIndex = dataProperty(s"${nifPrefix}endIndex")

  val subsense = objectProperty(s"${diamantSchemaPrefix}subsense")

  val senseOrder = dataProperty(s"${diamantSchemaPrefix}senseOrder")
  val isPolyLexical = dataProperty(s"${diamantSchemaPrefix}polyLexical")
  val isCoreSense = dataProperty(s"${diamantSchemaPrefix}isCoreSense")
  val senseLabel = dataProperty(s"${diamantSchemaPrefix}senseLabel")
  val gtbId = dataProperty(s"${diamantSchemaPrefix}gtbId")
  val gtbDictionary = dataProperty(s"${diamantSchemaPrefix}dictionary")
  val reference = objectProperty(s"${ontolexPrefix}reference")
  val senseDefinition = objectProperty(s"${lemonPrefix}definition")
  val definitionText = dataProperty(s"${diamantSchemaPrefix}definitionText")
  val quotationText = dataProperty(s"${diamantSchemaPrefix}quotation")

  val evokes = objectProperty(s"${ontolexPrefix}evokes")



  val rdfType = IRI(s"${rdfPrefix}type")

  val prefLabel = dataProperty(s"${skosPrefix}prefLabel")
  val altLabel = dataProperty(s"${skosPrefix}altLabel")
  val skosDefinition = objectProperty(s"${skosPrefix}definition")
  val skosBroader = objectProperty(s"${skosPrefix}broader")
  val skosNarrower = objectProperty(s"${skosPrefix}narrower")
  val skosRelated = objectProperty(s"${skosPrefix}related")
  val skosCloseMatch = objectProperty(s"${skosPrefix}closeMatch")
  val yearFrom = if (Settings.doLexCit) dataProperty(s"${diamantSchemaPrefix}notBefore") else dataProperty(s"${diamantSchemaPrefix}witnessYearFrom")
  val yearTo = if (Settings.doLexCit) dataProperty(s"${diamantSchemaPrefix}notAfter")else dataProperty(s"${diamantSchemaPrefix}witnessYearTo")
  val dcTitle = dataProperty(s"${dcTermsPrefix}title")
  val dcAuthor = dataProperty(s"${dcTermsPrefix}creator")


  val isA = rdfType

  // classes
  val senseDefinitionType = owlClass(s"${lemonPrefix}SenseDefinition")
  val entryType = owlClass(s"${ontolexPrefix}LexicalEntry")
  val conceptType = owlClass(s"${skosPrefix}Concept")
  val lexicalConceptType = owlClass(s"${ontolexPrefix}LexicalConcept")
  val formType = owlClass(s"${ontolexPrefix}Form")
  val attestationType = owlClass(s"${lexcitSchemaPrefix}Attestation")
  val locusType = owlClass(s"${lexcitSchemaPrefix}Locus")
  val senseType = owlClass(s"${ontolexPrefix}LexicalSense")
  val synonymDefinitionType = owlClass(s"${diamantSchemaPrefix}SynonymDefinition")
  val semanticRelationType  = owlClass(s"${diamantSchemaPrefix}SemanticRelation")
  val quotationType  = owlClass(s"${diamantSchemaPrefix}Quotation")
  val manifestationType  = owlClass(s"${frbrPrefix}Manifestation")
  val expressionType  = owlClass(s"${frbrPrefix}Expression")
  // resource prefixes

  val diamantBaseURI: String = INTBaseURI + "lexica/diamant/"
  val quotationResourcePrefix: String = diamantBaseURI + "quotation/"
  val expressionResourcePrefix: String = diamantBaseURI + "expression/"
  val senseResourcePrefix: String = diamantBaseURI + "sense/"
  val definitionResourcePrefix: String = diamantBaseURI + "definition/"
  val synonymDefinitionResourcePrefix: String = diamantBaseURI + "synonymdefinition/"
  val entryResourcePrefix: String = diamantBaseURI + "entry/"
  val canonicalFormResourcePrefix = diamantBaseURI + "canonicalform/"
  val attestationResourcePrefix: String = diamantBaseURI + "attestation/"
  val locusResourcePrefix: String = diamantBaseURI + "locus/"
  val synsetResourcePrefix: String = diamantBaseURI + "synset/"
  val wordformResourcePrefix: String = diamantBaseURI + "wordform/"
  val conceptResourcePrefix: String = diamantBaseURI + "concept/"
  val similarityResourcePrefix: String = diamantBaseURI + "similarity/"
  val semanticRelationResourcePrefix: String = diamantSchemaPrefix + "relation/" // relaties moeten in het schema komen

}
