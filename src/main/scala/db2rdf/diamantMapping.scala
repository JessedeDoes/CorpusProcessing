package db2rdf

import java.io.{FileOutputStream, FileWriter, OutputStreamWriter}
import java.sql.ResultSet
import java.util.zip.GZIPOutputStream

import database.Configuration
import db2rdf.Ω.{ϝ, ⊕}
import db2rdf.IRI
import org.slf4j._

/*
ToDo vergelijk met database thuis!!
Senses enzo van thuis halen, daar wel met nummers in de database zoals het moet?
 */

object posConversion {

  import java.util

  val knownPoS: List[String] = List("ADP", "ADV", "ADJ", "ART", "CONJ", "INT", "NOU-C", "NOU-P",
    "NOU-P_LOC", "NOU-P_ORG", "NOU-P_PER", "NOU-P_OTHER", "NUM", "PRN", "VRB", "COLL", "WOORDDL")

  val conversionTable = List(
    ("ADP", "ADP"),
    ("ADV", "ADV"),
    ("ADJ", "ADJ"),
    ("ART", "pos=PRON&PronType=Art"),
    ("CONJ", "SCONJ_CCONJ"),
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

  def convertPos(p: String): String = conversionTable.getOrElse(p, s"unknown")// conversionTable.getOrElse(p, s"unknown:$p")
}

object diamantMapping {

  import Ω._


  import commonDefinitions._
  ////// queries //////

  import Settings.{data_schema,sense_schema}

  val senseTable = s"$sense_schema.senses"

  val wordformQuery =
    s"""select * from $data_schema.lemmata l, $data_schema.analyzed_wordforms a, $data_schema.wordforms w
        where l.lemma_id=a.lemma_id and w.wordform_id=a.wordform_id"""

  val posQuery = s"""select wdb, persistent_id,regexp_split_to_table(lemma_part_of_speech,E'\\s+') as lemma_part_of_speech from $data_schema.lemmata"""

  val distinctPosQuery = """select distinct regexp_split_to_table(lemma_part_of_speech,E'\\s+|\\s*[+,]\\s*') as lemma_part_of_speech from data.lemmata"""

  val lemmaQuery = s"""select * from $data_schema.lemmata"""

  val attestationQuery: String =
    s"""select * from $data_schema.analyzed_wordforms a, $data_schema.token_attestations t, $data_schema.documents d
      | where
      |   a.analyzed_wordform_id = t.analyzed_wordform_id
      |   and d.document_id=t.document_id""".stripMargin

  val senseAttestationQueryOld: String =
    s"""select * from $data_schema.analyzed_wordforms a, $data_schema.token_attestations t, $data_schema.documents d
      | where
      |   a.analyzed_wordform_id = t.analyzed_wordform_id
      |   and d.document_id=t.document_id and t.sense_id is not null""".stripMargin

  val senseAttestationQuery: String =
    s"""select t.attestation_id, t.quote, t.start_pos, t.end_pos, sa.sense_id, sa.wdb from $data_schema.token_attestations t left join $sense_schema.sense_attestations_reloaded sa
      | on
      |   sa.quotation_section_id = t.quotation_section_id
      |   where sa.sense_id is not null""".stripMargin

  val documentQuery = s"select * from $data_schema.documents"

  val senseQuery = s"select * from $senseTable"

  val subSenseQuery = s"select * from $senseTable where parent_id is not null"

  val synonymQuery = s"select *, dictionary as wdb from $sense_schema.synonym_definitions where correct=true and synonym_modern_lemma is not null"

  val synsetQuery = "select synset_id, unnest(synset) as sense_id, 'WNT' as wdb from diamant.synsets"

  val synsetRelationQuery = "select parent_id, relation, child_id, 'WNT' as wdb  from diamant.synset_relations"

  val serpensConceptQuery = "select * from serpens.concepts"

  val serpensWntQuery: String =
    s"""select distinct serpens.concepts.*, $data_schema.lemmata.modern_lemma,
      |     $data_schema.lemmata.lemma_part_of_speech, $data_schema.lemmata.wdb, $data_schema.lemmata.persistent_id
      | from serpens.concepts,$data_schema.lemmata
      | where serpens.concepts.concept_id=$data_schema.lemmata.persistent_id""".stripMargin

  val conceptRelationQuery: String =
    """select distinct cl.*, s1.iri as parent_iri, s2.iri as child_iri
      | from serpens.concept_links cl, serpens.concepts s1, serpens.concepts s2
      | where s1.concept_id = cl.parent_id and s2.concept_id = cl.child_id""".stripMargin

  //////////////////////////////


  val lemma:ResultSet=>IRI = ~s"$entryResourcePrefix$$wdb/$$persistent_id"
  val lemmaFromSense:ResultSet=>IRI = ~s"$entryResourcePrefix$$wdb/$$lemma_id"

  val lemmata:Mappings = {
    val cf = ~s"$canonicalFormResourcePrefix$$wdb/$$persistent_id"
    ⊕(
      lemmaQuery,
      Ω(isA, lemma, entryType),
      Ω(isA, cf, formType),
      Ω(canonicalForm, lemma, cf),
      Δ(gtbId, lemma, !"persistent_id"),
      Δ(gtbDictionary, lemma, !"wdb"),
      Δ(writtenRep, cf, !"modern_lemma"))
  }

  val distinctPosMapping: Mappings = {
    def convertPoS(r: ResultSet): IRI = IRI(s"${udPrefix}pos/${posConversion.convertPos(r.getString("lemma_part_of_speech"))}")
    def convertedPoS(r: ResultSet) = {
      val udpos = posConversion.convertPos(r.getString("lemma_part_of_speech"))
      if (udpos.contains("unknown")) UndefinedLiteral("udpos")
      else StringLiteral(udpos, None)
    }

    ⊕(distinctPosQuery,
      Ω(isA, convertPoS, r => IRI(s"${udPrefix}part_of_speech")),
      Δ(rdfsLabel, convertPoS, convertedPoS))
  }

  val posMapping: Mappings = {
    def convertPoS(r: ResultSet): IRI = IRI(s"${udPrefix}pos/${posConversion.convertPos(r.getString("lemma_part_of_speech"))}")
    ⊕(posQuery,
      Ω(pos, ~s"$entryResourcePrefix$$wdb/$$persistent_id", convertPoS))
  }

  val lemmaWordform = {
    val awf = ~s"$wordformResourcePrefix$$wdb/$$analyzed_wordform_id"
    ⊕(wordformQuery,
      Ω(lexicalForm, lemma, awf),
      Ω(isA, awf, formType) ,
      Δ(writtenRep, awf, !"wordform"))
  }

  val senses: Mappings = { // je bent totaal vergeten lexical entries aan senses te koppelen; verder null als supersense als er geen supersense is
    // en soms foute wdb-indicatie in senses in database ....
    val sense = ~s"$senseResourcePrefix$$wdb/$$persistent_id"
    val definition = ~"http://rdf.ivdnt.org/definition/$wdb/$persistent_id"
    val nope:Mapping = NopeMapping()
    ⊕(
      senseQuery,
      Ω(commonDefinitions.sense, lemmaFromSense, sense),
      Ω(isA, sense, senseType),
      //Ω(subsense, ~s"$senseResourcePrefix$$wdb/$$parent_id", sense),
      Ω(senseDefinition, sense, definition),
      Ω(isA, definition, senseDefinitionType),
      if (!Settings.outputDefinitionsAndQuotations) nope else Δ(skosDefinition, sense, !"definition").asInstanceOf[Mapping],
      if (!Settings.outputDefinitionsAndQuotations) nope else Δ(definitionText, definition, !"definition"),
      Δ(rdfsLabel,  sense, !"path"),
      Δ(gtbId, sense, !"persistent_id"),
      Δ(gtbDictionary, sense, !"wdb"),
      Δ(isPolyLexical, sense, r => BooleanLiteral(r.getBoolean("is_verbinding"))),
      Δ(isCoreSense, sense, r => BooleanLiteral(r.getBoolean("is_core"))),
      Δ(senseOrder, sense, r => IntLiteral(r.getString("sense_order").toInt),
      )
      // Δ(senseLabel, sense, !"sense_label")
    )
  }

  val subsenses: Mappings = {
    val sense = ~s"$senseResourcePrefix$$wdb/$$persistent_id"

    ⊕(
      subSenseQuery,
      Ω(subsense, ~s"$senseResourcePrefix$$wdb/$$parent_id", sense)
    )
  }

  val synonyms = // ToDo doe de prov ellende hier ook nog....
  {
    val synonymDef = ~s"$synonymDefinitionResourcePrefix$$wdb/$$id"
    val sense = ~s"$senseResourcePrefix$$wdb/$$sense_id"
    ⊕(
      synonymQuery,
      Ω(senseDefinition, sense, synonymDef),
      Ω(isA, synonymDef, synonymDefinitionType),
      Δ(definitionText, synonymDef, !"synonym_modern_lemma"))
  }


  // ezeltjes en zo...
  val hilexRelMap:Map[String, String] = Map("=" -> "synonym", ">" -> "hyperonym", "<" -> "hyponym")

  val hilexSynsets: Mappings = {
    val synset = ~s"${synsetResourcePrefix}$$wdb/$$synset_id"
    ⊕(synsetQuery,
      Ω(isA, synset, lexicalConceptType),
      Ω(reference, ~s"${senseResourcePrefix}$$wdb/$$sense_id", synset))
  }

  val hilexSynsetRelations: Mappings =
  {
    val parent = ~s"${synsetResourcePrefix}$$parent_id"
    val child = ~s"${synsetResourcePrefix}$$child_id"

    val rel:ResultSet => IRI = r => {
      val relName = hilexRelMap.getOrElse(r.getString("relation"), "piep")
      IRI(s"$semanticRelationResourcePrefix$relName")
    }

    ⊕(synsetRelationQuery,
      Ω(rel, parent, child), Ω(isA, rel, semanticRelationType))
  }

  /*
  <http://example.org/anno13> a oa:Annotation ;
    oa:hasBody <http://example.org/review1> ;
    oa:hasTarget [
        oa:hasSource <http://example.org/ebook1> ;
        oa:hasSelector [
            a oa:TextPositionSelector ;
            oa:start 412 ;
            oa:end 795 ] ] .
   */

  val attestations: Mappings = {
    val theAttestation = ~s"$attestationResourcePrefix$$attestation_id"
    val theLocus = ~s"$locusResourcePrefix$$attestation_id"
    val theQuotation = ~s"$quotationResourcePrefix$$wdb/$$document_id" // doen we die nou nog?
    val nope:Mapping = NopeMapping()

    ⊕(
      attestationQuery,
      if (!Settings.miniDiamant) Ω(attestation, ~s"$wordformResourcePrefix$$wdb$$analyzed_wordform_id", theAttestation) else nope,
      Ω(hasCitedEntity, theAttestation, theQuotation),
      if (Settings.outputDefinitionsAndQuotations) Ω(diamantTextOfAttestation, theAttestation, theQuotation) else nope, // TODO haal weg in lexcit mode
      Ω(isA, theAttestation, attestationType),
      Ω(isA, theLocus, locusType),
      // Ω(attestation, ~s"$senseResourcePrefix$$wdb/$$sense_id", theAttestation), // hier zinloos?
      Ω(locus, theAttestation, theLocus),
      Δ(quotationText, theAttestation, !"quote"),
      Δ(beginIndex, theLocus, r => IntLiteral(r.getInt("start_pos"))),
      Δ(endIndex, theLocus, r => IntLiteral(r.getInt("end_pos"))))
  }

  val senseAttestations: Mappings = {
    val theAttestation = ~s"${attestationResourcePrefix}$$attestation_id"
    val theAnnotation = ~s"${annotationResourcePrefix}$$attestation_id"
    val theSelector = ~s"${selectorResourcePrefix}$$attestation_id"
    val theSource = ~s"${sourceResourcePrefix}$$attestation_id"
    val theTarget = ~s"${targetResourcePrefix}$$attestation_id"
    val quotation = ~s"${quotationResourcePrefix}$$wdb/$$document_id"
    val theSense = ~s"$senseResourcePrefix$$wdb/$$sense_id"

    val basics = List(Ω(attestation, theSense, theAttestation),
      Ω(hasCitingEntity, theAttestation, theSense))

    val oaStuff: Seq[Mapping] = if (Settings.miniDiamant) List(
      Ω(hasBody, theAnnotation, theSense),
      Ω(isA, theAnnotation, annotationType),
      Ω(hasTarget, theAnnotation, theTarget),
      Ω(hasSource, theTarget, theSource),
      Δ(rdfValue, theSource, !"quote"),
      Ω(hasSelector, theTarget, theSelector),
      Ω(isA, theSelector, textSelectorType),
      Δ(oaStart, theSelector, r => IntLiteral(r.getInt("start_pos"))),
      Δ(oaEnd, theSelector, r => IntLiteral(r.getInt("end_pos")))
    ) else List()
    ⊕(senseAttestationQuery,
      (basics ++  oaStuff):_*
    ) // dit lijkt 2 keer te gebeuren?
  }

  val quotations: Mappings = {
    val quotation = ~s"${quotationResourcePrefix}$$wdb/$$document_id" // ϝ("document_id", "http://document/" + _)
    val expression = ~s"${expressionResourcePrefix}$$wdb/$$document_id"
    ⊕(
      documentQuery,
      Ω(isA, quotation, quotationType),
      Ω(isA, quotation, manifestationType),
      Ω(isA, expression, expressionType),
      Ω(embodiment, expression, quotation),
      Ω(embodimentOf, quotation, expression),
      Δ(gtbId, quotation, !"persistent_id"),
      Δ(gtbDictionary, quotation, !"wdb"),
      Δ(yearFrom, quotation, r => IntLiteral(r.getInt("year_from"))),
      Δ(yearTo, quotation, r => IntLiteral(r.getInt("year_to"))),
      Δ(dcTitle, expression, !"title"),
      Δ(dcAuthor, expression, !"author") // wat doen we hier eigenlijk als iets niet gedefinieerd is....
    )
  }

  val typeForConcept: ResultSet => IRI =
    r => if (r.getString("ontology").equalsIgnoreCase("wnt")) lexicalConceptType else conceptType

  val serpensRelMap:Map[String, IRI] = Map("=" -> skosCloseMatch, ">" -> skosNarrower, "<" -> skosBroader)

  val serpensConcepts = {
    val concept = ~"$iri"
    ⊕(serpensConceptQuery,
      Ω(isA, concept, typeForConcept),
      Δ(prefLabel, concept, !"preflabel"),
      Δ(altLabel, concept, !"altlabel"))
  }

  val serpensWNT = { val concept = ~"$iri"
    ⊕(
    serpensWntQuery,
    Ω(isA, concept, typeForConcept),
    Ω(evokes, lemma, concept))
  }

  val serpensConceptRelations = {
    val parent = ~"$parent_iri"
    val child = ~"$child_iri"
    val rel:ResultSet => IRI = r => serpensRelMap.getOrElse(r.getString("relation"), "piep")
    ⊕(
      conceptRelationQuery,
      Ω(rel, parent, child))
  }

  val serpens = List(serpensConcepts, serpensWNT)
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


