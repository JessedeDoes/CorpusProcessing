package db2rdf

import java.io.FileInputStream
import java.sql.ResultSet

import org.postgresql.ds.PGPoolingDataSource
import org.skife.jdbi.v2.tweak.ResultSetMapper
import org.skife.jdbi.v2.{DBI, Handle, Query, StatementContext}

import scala.collection.JavaConverters._
import org.postgresql.core.BaseConnection
import com.mysql.jdbc.jdbc2.optional.MysqlDataSource
import com.mysql.jdbc.Driver
import database.DatabaseUtilities.AlmostQuery
import database.DatabaseUtilities.makeMapping
import database.DatabaseUtilities.ResultMapping
import database.Configuration
import net.sf.jsqlparser.schema.Database
trait Literal
trait Statement

case class StringLiteral(s: String) extends Literal { override def toString = '"' + s.toString + '"' }
case class IntLiteral(k: Int) extends Literal  { override def toString = k.toString   }
case class IRI(s: String)   { override def toString = '<' + s.toString + '>' }

case class ObjectProperty(s: IRI, p: IRI, o: IRI) extends Statement
{
  override def toString = s"$s $p $o ."
}

case class DataProperty(s: IRI, p: IRI, o: Literal) extends Statement
{
  override def toString = s"$s $p $o ."
}

trait Mapping

case class Ω(p: IRI, s: ResultSet => IRI, o: ResultSet => IRI) extends Mapping
{
  def triples(db: database.Database, q: String) : Stream[ObjectProperty] =
    {
      val query: AlmostQuery[ObjectProperty] = db => db.createQuery(q).map(ResultMapping(r => ObjectProperty(s(r) ,p, o(r))))
      db.stream(query)
    }
}
case class Δ(p: IRI, s: ResultSet => IRI, o: ResultSet => Literal) extends Mapping
{
  def triples(db: database.Database, q: String) : Stream[DataProperty] =
    {
      val query: AlmostQuery[DataProperty] = db => db.createQuery(q).map(ResultMapping(r => DataProperty(s(r) ,p, o(r))))
      db.stream(query)
    }
}

case class MultiMapping(mappings:Seq[Mapping])
{
  def multiMapping(mappings:Seq[Mapping]): ResultMapping[Set[Statement]] =
  {
    val dps = mappings.filter(_.isInstanceOf[Δ]).map(_.asInstanceOf[Δ]).toSet
    val ops = mappings.filter(_.isInstanceOf[Ω]).map(_.asInstanceOf[Ω]).toSet
    ResultMapping(r => ops.map(x => ObjectProperty(x.s(r) ,x.p, x.o(r))) ++ dps.map(x => DataProperty(x.s(r) ,x.p, x.o(r)))  )
  }

  def triples(db: database.Database, q: String): Stream[Statement] =
  {
    val m = multiMapping(this.mappings)
    val query : AlmostQuery[Set[Statement]] = db => db.createQuery(q).map(m)
    db.stream(query).flatten
  }
}



object Ω {
  implicit def x(s: String): ResultSet => IRI = r => IRI(r.getString(s))

  case class ϝ(field: String, f: String => String = identity)

  implicit def fd(x: (String, String => String)):ϝ = ϝ(x._1,x._2)

  implicit def y(d: ϝ): ResultSet => IRI = r => IRI(d.f(r.getString(d.field)))
  implicit def z(d: ϝ): ResultSet => StringLiteral = r => StringLiteral(d.f(r.getString(d.field)))
  implicit def z(x: (String, String => String)): ResultSet => StringLiteral = { val d:ϝ = x; r => StringLiteral(d.f(r.getString(d.field))) }
  implicit def i(s: String):IRI = IRI(s)

  case class XXX(s: String)
  {
    import scala.util.matching.Regex
    val f:ResultSet => StringLiteral = r =>  StringLiteral(r.getString(s))

    lazy val asTemplate: ResultSet => IRI  =
    {
      val varNames = "\\$([a-zA-Z][a-zA-Z0-9_]+)".r.findAllMatchIn(s).toStream.map(_.group(1))
      val z = if (varNames.nonEmpty)
        {
          val g:ResultSet => IRI = r =>
          {
            val substituted = varNames.foldLeft(s)( (z, v) => z.replaceAll("\\$" + v, r.getString(v)))
            val i = IRI(substituted)
            i
          }
          g
        } else
        {
          val g:ResultSet => IRI  = r => IRI("bla")
          g
        }
      z
    }

    def unary_~ = asTemplate
    def unary_! =f
  }

  implicit def xxx(s:String):XXX = XXX(s)

  def ⊕(l: Mapping*) = MultiMapping(l)

  ////////////////////////////////////////////////////////


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
    attestations.triples(db, attestationQuery).take(10).foreach(println)
  }
}
