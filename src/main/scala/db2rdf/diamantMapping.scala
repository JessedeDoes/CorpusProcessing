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
  val linguisticConceptType = owlClass(s"${diamantSchemaPrefix}LexicalConcept")
  val synonymDefinitionType = owlClass(s"${diamantSchemaPrefix}SynonymDefinition")
  ////// queries //////

  val wordformQuery =
    """select * from data.lemmata l, data.analyzed_wordforms a, data.wordforms w
        where l.lemma_id=a.lemma_id and w.wordform_id=a.wordform_id"""

  val posQuery = """select persistent_id,regexp_split_to_table(lemma_part_of_speech,E'\s+') as lemma_part_of_speech from data.lemmata"""

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

  val synonymQuery = "select * from diamant.synonym_definitions where correct=true"

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


  val senses: Mappings = {
    val sense = ~"http://rdf.ivdnt.org/sense/$persistent_id"
    val definition = ~"http://rdf.ivdnt.org/sense/$definition"
    ⊕(
      Ω(subsense, ~"http://rdf.ivdnt.org/sense/$parent_id", sense),
      Ω(senseDefinition, sense, definition),
      Δ(definitionText, definition, !"definition")
    )
  }

  val attestations: Mappings = {
    val theAttestation = ~"http://attestation/$attestation_id"
    val document = ~"http://quotation/$document_id"

    ⊕(
      Ω(attestation, ~"http://awf/$analyzed_wordform_id", theAttestation),
      Ω(text, theAttestation, document),
      Ω(attestation, ~"http://rdf.ivdnt.org/sense/$sense_id", theAttestation),
      Δ(beginIndex, theAttestation, r => IntLiteral(r.getInt("start_pos"))),
      Δ(endIndex, theAttestation, r => IntLiteral(r.getInt("end_pos")))
    )
  }

  val senseAttestations: Mappings = {
    val theAttestation = ~"http://attestation/$attestation_id"
    val document = ~"http://quotation/$document_id"

    ⊕(
      Ω(attestation, ~"http://rdf.ivdnt.org/sense/$sense_id", theAttestation)
    )
  }


  val documents: Mappings = {
    val d = ~"http://document/$document_id" // ϝ("document_id", "http://document/" + _)
    ⊕(
      Δ(yearFrom, d, !"year_from"),
      Δ(yearTo, d, !"year_to"),
      Δ(dcTitle, d, !"title"),
      Δ(dcAuthor, d, !"author")
    )
  }

  val posMapping: Mappings = {

  def convertPoS(r: ResultSet): IRI = {
    val p0 = r.getString("lemma_part_of_speech")
    val pUd = posConversion.convertPos(p0)
    IRI(s"http://universaldependencies.org/u/pos/$pUd")
  }
  ⊕(
    Ω(pos, ~"http//rdf.ivdnt.org/entry/$persistent_id", convertPoS)
  )}

  val lemma = ~"http//rdf.ivdnt.org/entry/$wdb/$persistent_id"

  val lemmata:Mappings =
    ⊕(
      Ω(canonicalForm, lemma , ~"http://rdf.ivdnt.org/canonical/$wdb/$lemma_id"),
      Δ(writtenRep, ~"http://rdf.ivdnt.org/canonical/$lemma_id", !"modern_lemma"),
    )


  val lemmaWordform =
  {
    val awf = ~"http://awf/$analyzed_wordform_id"
    ⊕(
      Ω(lexicalForm, ~"http//rdf.ivdnt.org/entry/$lemma_id", awf),
      Δ(writtenRep, awf, !"wordform")
    )
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

  val synonyms =
  {
    val synonymDef = ~"http//rdf.ivdnt.org/synonymdefinition/$id"
    ⊕(
      Ω(senseDefinition, ~"http//rdf.ivdnt.org/entry/$dictionary/$sense_id", synonymDef),
      Ω(isA, synonymDef, synonymDefinitionType),
      Δ(definitionText, senseDefinition, !"synonym")

      // ToDo doe de prov ellende hier ook nog....
    )
  }

  val typeForConcept: ResultSet => IRI =
    r => if (r.getString("ontology").equalsIgnoreCase("wnt")) linguisticConceptType else conceptType

  val serpensConcepts =
  {
    val concept = ~"$iri"
    ⊕(
      Ω(isA, concept, typeForConcept),
      Δ(prefLabel, concept, !"preflabel"),
      Δ(altLabel, concept, !"altlabel")
    )
  }


  val serpensWNT =
  {
    val concept = ~"$iri"

    ⊕(
      Ω(isA, concept, typeForConcept),
      Ω(evokes, lemma, concept)
    )
  }

  val relMap:Map[String, IRI] = Map("=" -> skosCloseMatch, ">" -> skosBroader, "<" -> skosNarrower)

  val serpensConceptRelations =
  {
    val parent = ~"$parent_iri"
    val child = ~"$child_iri"

    val rel:ResultSet => IRI = r => relMap.getOrElse(r.getString("relation"), "piep")

    ⊕(
      Ω(rel, parent, child),
    )
  }

  val allMappings = List(lemmata, lemmaWordform)
  val serpens = List(serpensConcepts, serpensWNT)

  val db = new database.Database(Configuration("x", "svprre02","gigant_hilex_clean", "postgres", "inl"))

  val limit = 1000
  def main(args: Array[String]) =
  {

    db.runStatement(("set schema 'data'"))

    lemmata.triples(db, lemmaQuery).take(limit).foreach(println)
    lemmaWordform.triples(db, wordformQuery).take(limit).foreach(println)
    serpensConcepts.triples(db, serpensConceptQuery).take(limit).foreach(println)
    serpensWNT.triples(db, serpensWntQuery).take(limit).foreach(println)
    serpensConceptRelations.triples(db, conceptRelationQuery).take(limit).foreach(println)

    synonyms.triples(db, synonymQuery).take(limit).foreach(println)
    posMapping.triples(db, posQuery).take(limit).foreach(println)



    senses.triples(db, senseQuery).take(limit).foreach(println)
    documents.triples(db, documentQuery).take(limit).foreach(println)
    senseAttestations.triples(db, senseAttestationQuery).take(limit).foreach(println)
    attestations.triples(db, attestationQuery).take(limit).foreach(println)
  }
}
