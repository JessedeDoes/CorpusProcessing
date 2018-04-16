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

case class mapping(p: IRI, s: ResultSet => IRI, o: ResultSet => IRI) extends Mapping
{
  def triples(db: database.Database, q: String) : Stream[ObjectProperty] =
    {
      val query: AlmostQuery[ObjectProperty] = db => db.createQuery(q).map(ResultMapping(r => ObjectProperty(s(r) ,p, o(r))))
      db.stream(query)
    }
}
case class mappingDP(p: IRI, s: ResultSet => IRI, o: ResultSet => Literal) extends Mapping
{
  def triples(db: database.Database, q: String) : Stream[DataProperty] =
    {
      val query: AlmostQuery[DataProperty] = db => db.createQuery(q).map(ResultMapping(r => DataProperty(s(r) ,p, o(r))))
      db.stream(query)
    }
}

case class MultiMapping(mappings:List[Mapping])
{
  def multiMapping(mappings:List[Mapping]): ResultMapping[Set[Statement]] =
  {
    val dps = mappings.filter(_.isInstanceOf[mappingDP]).map(_.asInstanceOf[mappingDP]).toSet
    val ops = mappings.filter(_.isInstanceOf[mapping]).map(_.asInstanceOf[mapping]).toSet
    ResultMapping(r => ops.map(x => ObjectProperty(x.s(r) ,x.p, x.o(r))) ++ dps.map(x => DataProperty(x.s(r) ,x.p, x.o(r)))  )
  }

  def triples(db: database.Database, q: String): Stream[Statement] =
  {
    val m = multiMapping(this.mappings)
    val query : AlmostQuery[Set[Statement]] = db => db.createQuery(q).map(m)
    db.stream(query).flatten
  }
}



object mapping {
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
      val x = "\\$([a-zA-Z][a-zA-Z0-9_]+)".r.findFirstMatchIn(s)
      val z = if (x.isDefined)
        {
          val g:ResultSet => IRI = r =>
          {
            val g = x.get.group(0); val y = r.getString(x.get.group(1));
            //Console.err.println(s"Hola $g=$y in $s!");
            val i = IRI(s.replaceAll("\\" + g, y))
            //Console.err.println(s"Dus: $i")
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
  ////// queries //////

  val wordformQuery = """select * from data.lemmata l, data.analyzed_wordforms a, data.wordforms w
        where l.lemma_id=a.lemma_id and w.wordform_id=a.wordform_id"""
  val posQuery = """select persistent_id,regexp_split_to_table(lemma_part_of_speech,'\s+') from data.lemmata"""

  val lemmaQuery = """select * from lemmata"""

  val attestationQuery =
    """select * from analyzed_wordforms a, token_attestations t, documents d
      | where
      |   a.analyzed_wordform_id = t.analyzed_wordform_id
      |   and d.document_id=t.document_id""".stripMargin

  val documentQuery = "select * from documents"

  val senseQuery = "select * from senses"

  //////////////////////////////


  val senses =
  {
    val sense = ~"http://rdf.ivdnt.org/sense/$persistent_id"
    val definition = ~"http://rdf.ivdnt.org/sense/$definition"
    MultiMapping(List(
      mapping(subsense, ~"http://rdf.ivdnt.org/sense/$parent_id", sense),
      mapping(lexicalDefinition, sense, definition),
      mappingDP(definitionText, definition, !"definition")
    ))
  }

  val attestations = {
    val theAttestation = ϝ("attestation_id", "http://attestation/" + _)
    val document = ϝ("document_id", "http://quotation/" + _)

    MultiMapping(List(
      mapping(attestation, ~"http://awf/$analyzed_wordform_id", theAttestation),
      mapping(text, theAttestation, document),
      mapping(attestation,  ~"http://rdf.ivdnt.org/sense/$sense_id", theAttestation),
      mappingDP(beginIndex, theAttestation, r => IntLiteral(r.getInt("start_pos"))),
      mappingDP(endIndex, theAttestation, r => IntLiteral(r.getInt("end_pos")))
    ))
  }

  val documents = {
    val d = ϝ("document_id", "http://document/" + _)
    MultiMapping(List(
      mappingDP("http://yearFrom", d, !"year_from"),
      mappingDP("http://yearTo", d, !"year_to"),
      mappingDP("http://title", d, !"title"),
      mappingDP("http://author", d, !"author")
    ))
  }

  val lemmata =
    MultiMapping(List(

      mapping(canonicalForm, ~"http//rdf.ivdnt.org/entry/$persistent_id", ~"http://rdf.ivdnt.org/canonical/$lemma_id"),
      mappingDP(writtenRep, ~"http://rdf.ivdnt.org/canonical/$lemma_id", !"modern_lemma"),

      // multiple PoS per entry? Or just separate query with split_to_table
      mapping(pos,
        ~"http//rdf.ivdnt.org/entry/$persistent_id",
        ~"http://universaldependencies.org/u/pos/$lemma_part_of_speech")
    ))


  val lemmaWordform =
    MultiMapping(List(

      mapping(lexicalForm,
        ϝ("lemma_id", "http//rdf.ivdnt.org/entry/" + _), ϝ("analyzed_wordform_id", "http://awf" + _)),

      mappingDP(writtenRep, ϝ("analyzed_wordform_id", "http://awf" + _), ϝ("wordform"))
    ))

  val allMappings = List(lemmata, lemmaWordform)




  val db = new database.Database(Configuration("x", "localhost","gigant_hilex_clean", "postgres", "inl"))

  def main(args: Array[String]) =
  {
    db.runStatement(("set schema 'data'"))
    allMappings.foreach(m =>
      m.triples(db, wordformQuery
      ).take(10).foreach(println)
    )

    senses.triples(db, senseQuery).take(10).foreach(println)
    documents.triples(db, documentQuery).take(10).foreach(println)

    attestations.triples(db, attestationQuery).take(10).foreach(println)


  }
}
