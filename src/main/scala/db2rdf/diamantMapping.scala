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

  val writtenRep = IRI("http://ontolex/writtenRep")
  val lexicalForm = IRI("http://ontolex/lexicalForm")
  val canonicalForm = IRI("http://ontolex/canonicalForm")
  val attestation = IRI("http://rdf.ivdnt.org/diamant/attestation")
  val text = IRI("http://rdf.ivdnt.org/diamant/text")
  val pos = IRI("http://universaldependencies.org/u/pos/")
  val beginIndex = IRI("http://nif/beginIndex")
  val endIndex = IRI("http://nif/endIndex")
  val subsense = IRI("http://rdf.ivdnt.org/diamant/subsense")
  val lexicalDefinition = IRI("http://rdf.ivdnt.org/diamant/lexicalDefinition")
  val definitionText = IRI("http://rdf.ivdnt.org/diamant/definitionText")
  val evokes = IRI("http://ontolex/evokes")
  val synonymDefinition = IRI("http://synonymDefinition")
  val rdfsType = IRI("http://rdfs/type")
  val prefLabel = IRI("http://skos/prefLabel")
  val altLabel = IRI("http://skos/altLabel")
  val skosBroader = IRI("http://skos/broader")
  val skosNarrower = IRI("http://skos/narrower")
  val skosRelated = IRI("http://skos/related")
  val skosCloseMatch = IRI("http://skos/closeMatch")
  val yearFrom = "http://yearFrom"
  val yearTo = "http://yearTo"
  val dcTitle = "http://title"
  val dcAuthor = "http://author"


  val isA = rdfsType

  val conceptType = IRI("http://skos/concept")
  val linguisticConceptType = IRI("http://rdf.ivdnt.org/LinguisticConcept")

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
      Ω(lexicalDefinition, sense, definition),
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
      Ω(synonymDefinition, ~"http//rdf.ivdnt.org/entry/$dictionary/$sense_id", synonymDef),
      Δ(definitionText, synonymDef, !"synonym")

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

  /*
   	val lemmaObject: Nothing = this.createLemma("wnt", db.string(r, "wnt_id"), lemma, db.string(r, "lemma_part_of_speech"))
					this.evokes(lemmaObject, concept)
   */
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

    val rel:ResultSet => IRI = r =>
      {
        val relName = r.getString("relation")
        relMap.getOrElse(relName, "piep")
      }

    ⊕(
      Ω(rel, parent, child),
    )
  }
/*
r -> {
					LexiconObject concept;
					String ontology = db.string(r, "ontology");


					if (ontology.equalsIgnoreCase("wnt"))
					   concept = this.model.createResource(db.string(r,"iri"), this.linguisticConceptType);
					else
					   concept = this.model.createResource(db.string(r,"iri"), this.conceptType);

					this.setLabel(concept, db.string(r,"altlabel"));
					this.setLabel(concept, db.string(r,"preflabel"));

				});
 */

  val allMappings = List(lemmata, lemmaWordform)
  val serpens = List(serpensConcepts, serpensWNT)

  val db = new database.Database(Configuration("x", "localhost","gigant_hilex_clean", "postgres", "inl"))

  def main(args: Array[String]) =
  {
    db.runStatement(("set schema 'data'"))

    lemmata.triples(db, lemmaQuery).take(10).foreach(println)
    serpensConcepts.triples(db, serpensConceptQuery).take(10).foreach(println)
    serpensWNT.triples(db, serpensWntQuery).take(10).foreach(println)
    serpensConceptRelations.triples(db, conceptRelationQuery).take(10).foreach(println)

    //System.exit(0)

    synonyms.triples(db, synonymQuery).take(10).foreach(println)
    posMapping.triples(db, posQuery).take(10).foreach(println)

    allMappings.foreach(m =>
      m.triples(db, wordformQuery
      ).take(10).foreach(println)
    )

    senses.triples(db, senseQuery).take(10).foreach(println)
    documents.triples(db, documentQuery).take(10).foreach(println)
    senseAttestations.triples(db, senseAttestationQuery).take(10).foreach(println)
    attestations.triples(db, attestationQuery).take(10).foreach(println)
  }
}
