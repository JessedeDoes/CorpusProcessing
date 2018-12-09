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

  //@throws[SQLException]
  //@throws[JSONException]

  case class Row(result: List[(String, Any)])

  def convert(rs: ResultSet) =
  {
    val rsmd = rs.getMetaData
    val numColumns = rsmd.getColumnCount

    val result = (0 until numColumns).toList.map( i => {
      val column_name = rsmd.getColumnName(i)
      val column_type = rsmd.getColumnType(i)
      val column_data = column_type match {
        case java.sql.Types.ARRAY => rs.getArray(column_name)
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
      (column_name -> JsonAST.JValue(column_data))
    })

    render(result)
  }
}
