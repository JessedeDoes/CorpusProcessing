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

case class StringLiteral(s: String) extends Literal;
case class IntLiteral(k: Int) extends Literal;
case class IRI(s: String)

case class ObjectProperty(s: IRI, p: IRI, o: IRI) extends Statement
case class DataProperty(s: IRI, p: IRI, o: Literal) extends Statement

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



object o {
  implicit def x(s: String): ResultSet => IRI = r => IRI(r.getString(s))

  case class ϝ(f: String => String, field: String)
  implicit def fd(x: (String, String => String)):ϝ = ϝ(x._2,x._1)

  implicit def y(d: ϝ): ResultSet => IRI = r => IRI(d.f(r.getString(d.field)))
  implicit def z(d: ϝ): ResultSet => StringLiteral = r => StringLiteral(d.f(r.getString(d.field)))
  implicit def z(x: (String, String => String)): ResultSet => StringLiteral = { val d:ϝ = x; r => StringLiteral(d.f(r.getString(d.field))) }
  implicit def i(s: String):IRI = IRI(s)

  val example =
    MultiMapping(List(
      mappingDP("http://diamant/hasLemma", ϝ("http//books/id/" + _, "lemma_id"), ϝ(identity, "modern_lemma")),
      mapping("http://diamant/lexicalForm", ϝ("http//books/id/" + _, "lemma_id"), ϝ("http://awf" + _, "analyzed_wordform_id")),
      mapping("writtenRep", ϝ("http://awf" + _, "analyzed_wordform_id"), ϝ(identity, "wordform"))
    ))

  val db = new database.Database(Configuration("x", "localhost","gigant_hilex_clean", "postgres", "inl"))

  def main(args: Array[String]) =
  {
    example.triples(db, "select * from data.lemmata l, data.analyzed_wordforms a, data.wordforms w where l.lemma_id=a.lemma_id and w.wordform_id=a.wordform_id").foreach(println)
  }
}
