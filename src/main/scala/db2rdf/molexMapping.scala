package db2rdf

import java.sql.ResultSet

import database.Configuration
import database.DatabaseUtilities.ResultMapping
import database.DatabaseUtilities.AlmostQuery
import db2rdf.Ω.{ϝ, ⊕}
import db2rdf.IRI
import posmapping.Feature

import scala.util.{Failure, Success, Try}
import org.slf4j._

object molexMapping {

  import Ω._

  import commonDefinitions._

  val wordformQuery =
    """select *, 'molex' as wdb
        from data.lemmata l, data.analyzed_wordforms a, data.wordforms w
        where l.lemma_id=a.lemma_id and w.wordform_id=a.wordform_id and l.keurmerk=true
          and is_parent=false and wordform_gigpos ~ '[A-Z]' and a.keurmerk=true"""

  val posQuery = """select wdb, persistent_id,regexp_split_to_table(lemma_part_of_speech,E'\s+') as lemma_part_of_speech from data.lemmata"""

  val lemmaQuery = """select *, 'molex' as wdb  from data.lemmata where keurmerk=true and is_parent=false"""

  val lemma: ResultSet => IRI = ~s"$entryResourcePrefix$$wdb/$$lemma_id"
  val canonical = ~s"$canonicalFormResourcePrefix$$wdb/$$lemma_id"


  val lemmata: Mappings =
    ⊕(
      lemmaQuery,
      Ω(canonicalForm, lemma, canonical),
      Ω(isA, lemma, entryType),
      Δ(writtenRep, canonical, !"modern_lemma"),
    )


  def posToUD(pos: String): Set[Feature] = {
    val pos1 = if (pos.contains("(")) pos else pos + "()"
    posmapping.molexTagMapping.mapTagCached(pos1)
  }

  def createPosFeaturesForLemma(r: ResultSet): List[Statement] = {
    Try(
      {
        val lemma_id = r.getString("lemma_id")

        posToUD(r.getString("lemma_gigpos")).map({
          case posmapping.Feature(name, value) =>
            if (name == "pos")
              ObjectProperty(s"${entryResourcePrefix}molex/$lemma_id", s"${udPrefix}$name", s"${udPrefix}$name/$value")
            else
              ObjectProperty(s"${entryResourcePrefix}molex/$lemma_id", s"${udPrefix}$name", s"${udPrefix}feat/$name.html#$value")
        }
        ).toList
      }) match {
      case Success(x) => x
      case Failure(f) => List.empty
    }
  }


  def createPosFeaturesForWordform(r: ResultSet): List[Statement] = {
    //Console.err.println(s"${r.getString("modern_lemma")} ${r.getString("wordform")}")
    Try(
      {
        val wordform_id = r.getString("analyzed_wordform_id")

        posToUD(r.getString("wordform_gigpos")).map({
          case posmapping.Feature(name, value) =>
            if (name == "pos")
              ObjectProperty(s"${wordformResourcePrefix}molex/$wordform_id", s"${udPrefix}$name", s"${udPrefix}$name/$value")
            else
              ObjectProperty(s"${wordformResourcePrefix}molex/$wordform_id", s"${udPrefix}$name", s"${udPrefix}feat/$name.html#$value")
        }
        ).toList
      })
    match {
      case Success(x) => x
      case Failure(f) => List.empty
    }
  }


  lazy val lemmaPosQuery: AlmostQuery[List[Statement]] = db => db.createQuery(lemmaQuery).map(ResultMapping(createPosFeaturesForLemma))
  lazy val wordformPosQuery: AlmostQuery[List[Statement]] = db => db.createQuery(wordformQuery).map(ResultMapping(createPosFeaturesForWordform))

  val lemmaWordform = {
    val awf = ~s"${wordformResourcePrefix}molex/$$analyzed_wordform_id"
    ⊕(
      wordformQuery,
      Ω(isA, awf, formType),
      Ω(lexicalForm, lemma, awf),
      Δ(writtenRep, awf, !"wordform"),
      Δ(hyphenation, awf, !"wordform_afbr") // ahem, nee, die zit bij woordvorm
    )
  }

  val db = new database.Database(Configuration("x", "svowdb06", "gig_pro", "fannee", "Cric0topus"))

  val limit = Int.MaxValue

  def main(args: Array[String]) = {
    db.runStatement(("set schema 'data'"))

    db.stream(lemmaPosQuery).flatten.foreach(println)
    db.stream(wordformPosQuery).flatten.foreach(println)

    lemmata.triplesStream(db, lemmaQuery).take(limit).foreach(println)
    lemmaWordform.triplesStream(db, wordformQuery).take(limit).foreach(println)
  }
}
