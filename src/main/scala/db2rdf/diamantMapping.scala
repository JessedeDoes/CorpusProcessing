package db2rdf

import java.sql.ResultSet

import database.Configuration
import db2rdf.Ω.{ϝ, ⊕}
import db2rdf.IRI


object posConversion {

  import java.util

  val knownPoS: List[String] = List("ADP", "ADV", "ADJ", "ART", "CONJ", "INT", "NOU-C", "NOU-P",
    "NOU-P_LOC", "NOU-P_ORG", "NOU-P_PER", "NOU-P_OTHER", "NUM", "PRN", "VRB", "COLL", "WOORDDL")

  val conversionTable = List(
    ("ADP", "ADP"),
    ("ADV", "ADV"),
    ("ADJ", "ADJ"),
    ("ART", "pos=PRON&PronType=Art"),
    ("CONJ", "SCONJ|CCONJ"),
    ("INT", "INTJ"),
    ("NOU", "NOUN"),
    ("NOU-C", "NOUN"),
    ("NOU-P", "PROPN"),
    ("NOU-P_LOC", "PROPN"),
    ("NOU-P_ORG", "PROPN"),
    ("NOU-P_PER", "PROPN"),
    ("NOU-P_OTHER", "PROPN"),
    ("NUM", "NUM"),
    ("PRN", "PRON"),
    ("VRB", "VERB")).toMap

  def convertPos(p: String): String = conversionTable.getOrElse(p, s"unknown:$p")
}

object diamantMapping {

  import Ω._


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

  ////// queries //////

  val wordformQuery =
    """select * from data.lemmata l, data.analyzed_wordforms a, data.wordforms w
        where l.lemma_id=a.lemma_id and w.wordform_id=a.wordform_id"""

  val posQuery = """select wdb, persistent_id,regexp_split_to_table(lemma_part_of_speech,E'\s+') as lemma_part_of_speech from data.lemmata"""

  val lemmaQuery = """select * from lemmata"""

  val attestationQuery: String =
    """select * from analyzed_wordforms a, token_attestations t, documents d
      | where
      |   a.analyzed_wordform_id = t.analyzed_wordform_id
      |   and d.document_id=t.document_id""".stripMargin

  val senseAttestationQuery: String =
    """select * from analyzed_wordforms a, token_attestations t, documents d
      | where
      |   a.analyzed_wordform_id = t.analyzed_wordform_id
      |   and d.document_id=t.document_id and t.sense_id is not null""".stripMargin

  val documentQuery = "select * from documents"

  val senseQuery = "select * from senses"

  val synonymQuery = "select *, 'MNW' as wdb from diamant.synonym_definitions where correct=true"

  val synsetQuery = "select synset_id, unnest(synset) as sense_id, 'WNT' as wdb from diamant.synsets"

  val synsetRelationQuery = "select parent_id, relation, child_id, 'WNT' as wdb  from diamant.synset_relations"

  val serpensConceptQuery = "select * from serpens.concepts"

  val serpensWntQuery: String =
    """select serpens.concepts.*, data.lemmata.modern_lemma, data.lemmata.lemma_part_of_speech, data.lemmata.wdb, data.lemmata.persistent_id
      | from serpens.concepts,data.lemmata
      | where serpens.concepts.concept_id=data.lemmata.persistent_id""".stripMargin

  val conceptRelationQuery: String =
    """select cl.*, s1.iri as parent_iri, s2.iri as child_iri
      | from serpens.concept_links cl, serpens.concepts s1, serpens.concepts s2
      | where s1.concept_id = cl.parent_id and s2.concept_id = cl.child_id""".stripMargin

  //////////////////////////////


  val lemma:ResultSet=>IRI = ~s"$entryResourcePrefix$$wdb/$$persistent_id"

  val lemmata:Mappings =
    ⊕(Ω(canonicalForm, lemma , ~"http://rdf.ivdnt.org/canonical/$wdb/$lemma_id"),
      Δ(writtenRep, ~s"$canonicalFormResourcePrefix$$wdb/$$persistent_id", !"modern_lemma"))

  val posMapping: Mappings = {
    def convertPoS(r: ResultSet): IRI = IRI(s"$udPrefix/pos/${posConversion.convertPos(r.getString("lemma_part_of_speech"))}")
    ⊕(Ω(pos, ~s"$entryResourcePrefix$$wdb/$$persistent_id", convertPoS))
  }

  val lemmaWordform = {
    val awf = ~s"$wordformResourcePrefix$$analyzed_wordform_id"
    ⊕(Ω(lexicalForm, lemma, awf), Δ(writtenRep, awf, !"wordform"))
  }

  val senses: Mappings = {
    val sense = ~s"$senseResourcePrefix$$wdb/$$persistent_id"
    val definition = ~"http://rdf.ivdnt.org/definition/$wdb/$persistent_id"
    ⊕(
      Ω(subsense, ~s"$senseResourcePrefix$$wdb/$$parent_id", sense),
      Ω(senseDefinition, sense, definition),
      Δ(definitionText, definition, !"definition"))
  }

  val synonyms = // ToDo doe de prov ellende hier ook nog....
  {
    val synonymDef = ~s"$synonymDefinitionResourcePrefix$$wdb/$$id"
    val sense = ~s"$senseResourcePrefix$$wdb/$$sense_id"
    ⊕(
      Ω(senseDefinition, sense, synonymDef),
      Ω(isA, synonymDef, synonymDefinitionType),
      Δ(definitionText, senseDefinition, !"synonym"))
  }


  // ezeltjes en zo...
  val hilexRelMap:Map[String, String] = Map("=" -> "synonym", ">" -> "hyperonym", "<" -> "hyponym")

  val hilexSynsets: Mappings = {
    val synset = ~s"${synsetResourcePrefix}$$wdb/$$synset_id"
    ⊕(Ω(isA, synset, lexicalConceptType), Ω(reference, ~s"${senseResourcePrefix}$$wdb/$$sense_id", synset))
  }

  val hilexSynsetRelations: Mappings =
  {
    val parent = ~s"${synsetResourcePrefix}$$parent_id"
    val child = ~s"${synsetResourcePrefix}$$child_id"

    val rel:ResultSet => IRI = r => {
      val relName = hilexRelMap.getOrElse(r.getString("relation"), "piep")
      IRI(s"$semanticRelationResourcePrefix$relName")
    }

    ⊕(Ω(rel, parent, child), Ω(isA, rel, semanticRelationType))
  }

  val attestations: Mappings = {
    val theAttestation = ~s"$attestationResourcePrefix$$attestation_id"
    val document = ~"http://quotation/$document_id"

    ⊕(Ω(attestation, ~s"${wordformResourcePrefix}$$analyzed_wordform_id", theAttestation),
      Ω(text, theAttestation, document),
      Ω(attestation, ~s"$senseResourcePrefix$$sense_id", theAttestation),
      Δ(beginIndex, theAttestation, r => IntLiteral(r.getInt("start_pos"))),
      Δ(endIndex, theAttestation, r => IntLiteral(r.getInt("end_pos"))))
  }

  val senseAttestations: Mappings = {
    val theAttestation = ~s"${attestationResourcePrefix}$$attestation_id"
    val quotation = ~s"${quotationResourcePrefix}$$document_id"

    ⊕(Ω(attestation, ~s"$senseResourcePrefix$$sense_id", theAttestation))
  }

  val quotations: Mappings = {
    val quotation = ~s"${quotationResourcePrefix}$$wdb/$$document_id"// ϝ("document_id", "http://document/" + _)
    ⊕(
      Δ(yearFrom, quotation, r => IntLiteral(r.getInt("year_from"))),
      Δ(yearTo, quotation, r => IntLiteral(r.getInt("year_to"))),
      Δ(dcTitle, quotation, !"title"),
      Δ(dcAuthor, quotation, !"author")
    )
  }

  val typeForConcept: ResultSet => IRI =
    r => if (r.getString("ontology").equalsIgnoreCase("wnt")) lexicalConceptType else conceptType

  val serpensRelMap:Map[String, IRI] = Map("=" -> skosCloseMatch, ">" -> skosBroader, "<" -> skosNarrower)

  val serpensConcepts = {
    val concept = ~"$iri"
    ⊕(Ω(isA, concept, typeForConcept), Δ(prefLabel, concept, !"preflabel"), Δ(altLabel, concept, !"altlabel"))
  }

  val serpensWNT = { val concept = ~"$iri"; ⊕(Ω(isA, concept, typeForConcept), Ω(evokes, lemma, concept)) }

  val serpensConceptRelations = {
    val parent = ~"$parent_iri"
    val child = ~"$child_iri"
    val rel:ResultSet => IRI = r => serpensRelMap.getOrElse(r.getString("relation"), "piep")
    ⊕(Ω(rel, parent, child))
  }

  val serpens = List(serpensConcepts, serpensWNT)

  val db = new database.Database(Configuration("x", "svprre02","gigant_hilex_clean", "postgres", "inl"))

  val limit = Int.MaxValue

  def main(args: Array[String]) =
  {
    db.runStatement(("set schema 'data'"))

    println(s"#lemmata en PoS voor ${lemmata.triples(db, lemmaQuery).size} lemmata")

    lemmata.triples(db, lemmaQuery).take(limit).foreach(println)
    posMapping.triples(db, posQuery).take(limit).foreach(println)

    println(s"#woordvormen ${lemmaWordform.triples(db, wordformQuery).size} triples")

    lemmaWordform.triples(db, wordformQuery).take(limit).foreach(println)

    println(s"#senses synonym stuk: ${synonyms.triples(db, synonymQuery).size}")

    senses.triples(db, senseQuery).take(limit).foreach(println)
    synonyms.triples(db, synonymQuery).take(limit).foreach(println)

    println("#attestations en quotations")

    quotations.triples(db, documentQuery).take(limit).foreach(println)
    senseAttestations.triples(db, senseAttestationQuery).take(limit).foreach(println)
    attestations.triples(db, attestationQuery).take(limit).foreach(println)

    println("#ezels")

    hilexSynsets.triples(db, synsetQuery).take(limit).foreach(println)
    hilexSynsetRelations.triples(db, synsetRelationQuery).take(limit).foreach(println)

    println("#serpens")

    serpensConcepts.triples(db, serpensConceptQuery).take(limit).foreach(println)
    serpensWNT.triples(db, serpensWntQuery).take(limit).foreach(println)
    serpensConceptRelations.triples(db, conceptRelationQuery).take(limit).foreach(println)
  }
}

/*
import net.xqj.basex.bin.r
rel.id = r.getInt("id")// todo better id's (more persistent) for this

      rel.dictionary = r.getString("dictionary")
      rel.lemmaId = r.getString("entry_id")
      rel.senseId = r.getString("sense_id").trim
      rel.synonym = r.getString("synonym")
      rel.correct = r.getBoolean("correct")
      rel.extra = r.getBoolean("extra")
      rel.verified = r.getBoolean("verified")
 */


