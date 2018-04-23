package db2rdf

import database.Configuration
import db2rdf.Ω.{ϝ, ⊕}

object diamantMapping {
  import Ω._

  val writtenRep = "http://ontolex/writtenRep"
  val lexicalForm = "http://ontolex/lexicalForm"
  val canonicalForm = "http://ontolex/canonicalForm"
  val attestation = "http://rdf.ivdnt.org/diamant/attestation"
  val text = "http://rdf.ivdnt.org/diamant/text"
  val pos = "http://universaldependencies.org/u/pos/"
  val beginIndex = "http://nif/beginIndex"
  val endIndex = "http://nif/endIndex"
  val subsense = "http://rdf.ivdnt.org/diamant/subsense"
  val lexicalDefinition = "http://rdf.ivdnt.org/diamant/lexicalDefinition"
  val definitionText = "http://rdf.ivdnt.org/diamant/definitionText"
  val synonymDefinition = "http://synonymDefinition"
  ////// queries //////

  val wordformQuery = """select * from data.lemmata l, data.analyzed_wordforms a, data.wordforms w
        where l.lemma_id=a.lemma_id and w.wordform_id=a.wordform_id"""

  val posQuery = """select persistent_id,regexp_split_to_table(lemma_part_of_speech,E'\s+') as lemma_part_of_speech from data.lemmata"""

  val lemmaQuery = """select * from lemmata"""

  val attestationQuery =
    """select * from analyzed_wordforms a, token_attestations t, documents d
      | where
      |   a.analyzed_wordform_id = t.analyzed_wordform_id
      |   and d.document_id=t.document_id""".stripMargin

  val senseAttestationQuery =
    """select * from analyzed_wordforms a, token_attestations t, documents d
      | where
      |   a.analyzed_wordform_id = t.analyzed_wordform_id
      |   and d.document_id=t.document_id and t.sense_id is not null""".stripMargin

  val documentQuery = "select * from documents"

  val senseQuery = "select * from senses"

  val synonymQuery = "select * from diamant.synonym_definitions where correct=true"
  //////////////////////////////


  val senses:MultiMapping =
  {
    val sense = ~"http://rdf.ivdnt.org/sense/$persistent_id"
    val definition = ~"http://rdf.ivdnt.org/sense/$definition"
    ⊕(
      Ω(subsense, ~"http://rdf.ivdnt.org/sense/$parent_id", sense),
      Ω(lexicalDefinition, sense, definition),
      Δ(definitionText, definition, !"definition")
    )
  }

  val attestations:MultiMapping = {
    val theAttestation = ~"http://attestation/$attestation_id"
    val document = ~"http://quotation/$document_id"

    ⊕(
      Ω(attestation, ~"http://awf/$analyzed_wordform_id", theAttestation),
      Ω(text, theAttestation, document),
      Ω(attestation,  ~"http://rdf.ivdnt.org/sense/$sense_id", theAttestation),
      Δ(beginIndex, theAttestation, r => IntLiteral(r.getInt("start_pos"))),
      Δ(endIndex, theAttestation, r => IntLiteral(r.getInt("end_pos")))
    )
  }

  val senseAttestations:MultiMapping = {
    val theAttestation = ~"http://attestation/$attestation_id"
    val document = ~"http://quotation/$document_id"

    ⊕(
      Ω(attestation,  ~"http://rdf.ivdnt.org/sense/$sense_id", theAttestation)
    )
  }

  val documents:MultiMapping = {
    val d = ϝ("document_id", "http://document/" + _)
    ⊕(
      Δ("http://yearFrom", d, !"year_from"),
      Δ("http://yearTo", d, !"year_to"),
      Δ("http://title", d, !"title"),
      Δ("http://author", d, !"author")
    )
  }

  val posMapping:MultiMapping =
    ⊕(
      Ω(pos, ~"http//rdf.ivdnt.org/entry/$persistent_id",  ~"http://universaldependencies.org/u/pos/$lemma_part_of_speech")
    )

  val lemmata:MultiMapping =
    ⊕(
      Ω(canonicalForm, ~"http//rdf.ivdnt.org/entry/$persistent_id", ~"http://rdf.ivdnt.org/canonical/$lemma_id"),
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

      // ToDo doe de prov ellende hier ook
    )
  }


  val allMappings = List(lemmata, lemmaWordform)

  val db = new database.Database(Configuration("x", "localhost","gigant_hilex_clean", "postgres", "inl"))

  def main(args: Array[String]) =
  {
    db.runStatement(("set schema 'data'"))

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
