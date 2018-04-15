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
trait Literal;
case class StringLiteral(s: String) extends Literal;
case class IntLiteral(k: Int) extends Literal;
case class IRI(s: String)
case class ObjectProperty(s: IRI, p: IRI, o: IRI)
case class DataProperty(s: IRI, p: IRI, o: Literal)

case class mapping(q: String, p: IRI, s: ResultSet => IRI, o: ResultSet => IRI)
{
  val query: AlmostQuery[ObjectProperty] = db => db.createQuery(q).map(ResultMapping(r => ObjectProperty(s(r) ,p, o(r))))
  def triples(db: database.Database) : Stream[ObjectProperty] = db.stream(query)
}
case class mappingDP(q: String, p: IRI, s: ResultSet => IRI, o: ResultSet => Literal)
{
  val query: AlmostQuery[DataProperty] = db => db.createQuery(q).map(ResultMapping(r => DataProperty(s(r) ,p, o(r))))
  def triples(db: database.Database) : Stream[DataProperty] = db.stream(query)
}


object o {
  implicit def x(s: String): ResultSet => IRI = r => IRI(r.getString(s))

  case class fieldDef(f: String => String, field: String)
  implicit def fd(x: (String, String => String)):fieldDef = fieldDef(x._2,x._1)

  implicit def y(d: fieldDef): ResultSet => IRI = r => IRI(d.f(r.getString(d.field)))
  implicit def z(d: fieldDef): ResultSet => StringLiteral = r => StringLiteral(d.f(r.getString(d.field)))
  implicit def i(s: String):IRI = IRI(s)

  val example = mappingDP("select * from data.lemmata",
    "hasLemma",
    fieldDef("http//books/id/" + _, "lemma_id"),
    fieldDef(identity, "modern_lemma"))

  val db = new database.Database(Configuration("x", "localhost","gigant_hilex_clean", "postgres", "inl"))

  def main(args: Array[String]) =
  {
    example.triples(db).foreach(println)
  }
}
