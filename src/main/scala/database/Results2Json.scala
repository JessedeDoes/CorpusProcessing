package database

import java.io.FileInputStream
import java.sql.ResultSet

import org.postgresql.ds.PGPoolingDataSource
import org.skife.jdbi.v2.tweak.ResultSetMapper
import org.skife.jdbi.v2.{DBI, Handle, Query, StatementContext}

import scala.collection.JavaConverters._
import org.postgresql.core.BaseConnection
import com.mysql.jdbc.jdbc2.optional.MysqlDataSource
import com.mysql.jdbc.Driver
import database.DatabaseUtilities.{AlmostQuery, ResultMapping}
import db2rdf.ObjectProperty

//import org.json.JSONArray
///import org.json.JSONObject
//import org.json.JSONException
import java.sql.SQLException
import java.sql.ResultSet
import java.sql.ResultSetMetaData

import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._



object Results2Json {

  import scala.util.parsing.json.JSONArray
  import scala.util.parsing.json.JSONObject
  import java.sql.SQLException

  import org.json4s._
  import org.json4s.JsonDSL._
  import org.json4s.jackson.JsonMethods._
  import org.json4s.Extraction

  def encodeJson(src: Any): JValue = {
    import org.json4s.{ Extraction, NoTypeHints }
    import org.json4s.JsonDSL.WithDouble._
    import org.json4s.jackson.Serialization
    implicit val formats = Serialization.formats(NoTypeHints)

    Extraction.decompose(src)
  }

  //@throws[SQLException]
  //@throws[JSONException]

  case class Row(result: List[(String, Any)])

  def convert(rs: ResultSet) =
  {
    val rsmd = rs.getMetaData
    val numColumns = rsmd.getColumnCount

    val result = (1 to numColumns).toList.map( i => {
      val column_name = rsmd.getColumnName(i)
      val column_type = rsmd.getColumnType(i)
      val column_data = column_type match {
        case java.sql.Types.ARRAY => rs.getArray(column_name).getArray()
        case java.sql.Types.BIGINT => rs.getInt(column_name)
        case java.sql.Types.BOOLEAN => rs.getBoolean(column_name)
        case java.sql.Types.BLOB => rs.getBlob(column_name)
        case java.sql.Types.DOUBLE => rs.getDouble(column_name)
        case java.sql.Types.FLOAT => rs.getFloat(column_name)
        case java.sql.Types.INTEGER => rs.getInt(column_name)
        case java.sql.Types.NVARCHAR => rs.getNString(column_name)
        case java.sql.Types.VARCHAR => rs.getString(column_name)
        case java.sql.Types.TINYINT => rs.getInt(column_name)
        case java.sql.Types.SMALLINT => rs.getInt(column_name)
        case java.sql.Types.DATE => rs.getDate(column_name)
        case java.sql.Types.TIMESTAMP => rs.getTimestamp(column_name)
        case _ => rs.getObject(column_name)
      }
      (column_name -> encodeJson(column_data))
    })

    render(result)
  }

  def queryJson(q: String):AlmostQuery[JValue] = db => db.createQuery(q).map(ResultMapping(convert))

  def resultsAsJson(db: Database, q: String, schema: Option[String]=None) =
  {
    if (schema.isDefined) db.runStatement(s"set schema '${schema.get}';")
    val x = ("results" -> db.slurp(queryJson(q))) ~ ("query" -> q) ~ ("database" -> db.getConfiguration.toString)
    render(x)
  }

  def resultsAsJsonString(db: Database, q: String, s:Option[String]=None) = compact(resultsAsJson(db,q,s))


  //val testje = Configuration(name="oefen", server="localhost", database="oefen", user="postgres", password="postgres")

  //val testDB = new Database(testje)

  val testje = Configuration(name="oefen", server="localhost", database="oefen", user="postgres", password="postgres")
  val dsdd = testje.copy(server="svowdb02", database="hercules",password="inl")
  val testDB = new Database(dsdd)

  val qTest = queryJson("select * from bla")

  val gisTest =
    """
      |select max(keywords.lemma) as lemma,
      |    string_agg(distinct keywords.dictionary, ',') as dict,
      |    max(keywords.keyword) as keyword,
      |    max(keywords.definition) as definition,
      |    string_agg(distinct location_area, ', ') as area,
      |    string_agg(distinct location_subarea, ', ') as subarea,
      |    string_agg(distinct provincie, ', ') as province,
      |    string_agg(distinct streek, ', ') as region,
      |    string_agg(distinct land, ', ') as country,
      |    string_agg(distinct location_place, ', ') as place,
      |    string_agg(distinct kloeke_new,', ') as kloeke,
      |    string_agg(distinct lng || ',' || lat, '; ') as points
      |from integratie.keywords keywords, integratie.union_table union_table, gis.kloeke_cumul coords
      |where (keywords.keyword='kikker' or keywords.lemma='kikker')
      | and  union_table.lemma_id=keywords.lemma_id
      | and union_table.keyword=keywords.keyword_org
      | and coords.kloeke_code1=union_table.kloeke_new
      | and st_contains( st_setsrid(ST_GEOMFROMTEXT('polygon((2.4446895211046775 50.581492622208735, 2.999461692166681 50.581492622208735, 2.999461692166681 50.78336517951859, 2.4446895211046775 50.78336517951859,  2.4446895211046775 50.581492622208735))'),4326), point)
      | group by keywords.lemma_id, keyword_id;
      |
    """.stripMargin
  def main(args: Array[String]): Unit = {
    //testDB.slurp(qTest).foreach(x => println(compact(x)))
    val jResults = resultsAsJson(testDB, gisTest, Some("gis"))
    println(compact(jResults))
  }
}
