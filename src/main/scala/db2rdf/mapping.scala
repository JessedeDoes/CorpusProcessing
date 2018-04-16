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

  ////////////////////////////////////////////////////////


  val writtenRep = "http://ontolex/writtenRep"
  val lexicalForm = "http://ontolex/lexicalForm"
  val canonicalForm = "http://ontolex/canonicalForm"
  val pos = "http://universaldependencies.org/u/pos/"

  val lemmata =
    MultiMapping(List(

      mapping(canonicalForm,
        ϝ("persistent_id", "http//rdf.ivdnt.org/entry/" + _),
        ϝ("lemma_id", "http://canonical/" + _)),

      mappingDP(writtenRep, ϝ("lemma_id", "http://canonical/" + _), ϝ("modern_lemma")),

      mapping(pos,
        ϝ("persistent_id", "http//rdf.ivdnt.org/entry/" + _),
        ϝ("lemma_part_of_speech", pos + _))

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
    allMappings.foreach(m =>
    m.triples(db,
      "select * from data.lemmata l, data.analyzed_wordforms a, data.wordforms w where l.lemma_id=a.lemma_id and w.wordform_id=a.wordform_id").take(100).foreach(println)
    )
  }
}
