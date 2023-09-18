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

case class Configuration(name: String, server: String, database: String, user: String, password: String, driver: String="postgres")



object DatabaseUtilities
{
  type AlmostQuery[T] = (Handle => Query[T])

  implicit def makeMapping[T](f: ResultSet => T) = ResultMapping[T](f)

  case class ResultMapping[T](f: ResultSet => T) extends ResultSetMapper[T]
  {
    override def map(arg0: Int, r: ResultSet, arg2: StatementContext): T =
    {
      val w  = f(r)
      w
    }
  }

  case class Select[T](mapping: Diamond => T, from: String)
  {

  }

  case class Voedsel(beest:String, voedsel:Int)

  def toArray(a: java.sql.Array): Array[String]  =  {
    val a0= a.getArray().asInstanceOf[Array[String]]
    // println(a0.getClass.getName)
    return a0
  }

  trait Diamond
  {
    def getString(s:String, s1: Option[String] = None):String
    def getStringNonNull(s:String):String
    def getInt(s:String):Int
    def getBoolean(s:String):Boolean
    def getStringArray(s: String):Array[String]
  }

  case class Mocky1(resultSet:ResultSet) extends Diamond
  {
    def getString(s:String, s1: Option[String] = None):String =  {
      // Console.err.println(s"getString: $s $s1")
      resultSet.getString(s)
    }
    def getStringNonNull(s:String):String = {
      val x = resultSet.getString(s)
      if (x == null) "" else x
    }
    def getInt(s:String):Int = resultSet.getInt(s)
    def getBoolean(s:String):Boolean = resultSet.getBoolean(s)
    def getStringArray(s: String):Array[String] =toArray(resultSet.getArray(s))
  }

  class Mocky2 extends Diamond
  {
    val fieldNames: scala.collection.mutable.ListBuffer[String] = new scala.collection.mutable.ListBuffer[String]()
    def getString(s:String, s1: Option[String] = None):String = {
      Console.err.println(s"getString in Mocky2: $s $s1")
      fieldNames.append(s1.getOrElse("\"" + s + "\"")); s"wereldvrede_$s"}
    def getStringNonNull(s:String):String = getString(s)
    def getInt(s:String):Int = {fieldNames.append(s); 42}
    def getBoolean(s:String):Boolean = {fieldNames.append(s); true}
    override def getStringArray(s: String): Array[String] = {fieldNames.append(s); Array("piep")}
  }

  def slurp[A](db: Handle, a: AlmostQuery[A]):List[A] =
  {
    a(db).list().asScala.toList
  }

  def stream[A](db: Handle, a: AlmostQuery[A]):Stream[A] =
  {
    a(db).iterator().asScala.toStream
  }

  def iterator[A](db: Handle, a: AlmostQuery[A]):Iterator[A] =
  {
    a(db).iterator().asScala
  }

  implicit def doeHet[T](s:Select[T]): AlmostQuery[T] =
  {
    val m = new Mocky2
    Console.err.println("!!!!Creating query for $s " + s.mapping(m))
    val gr = ResultMapping[T](r => s.mapping(Mocky1(r)))
    val query = "select " + m.fieldNames.mkString(", ") + " from " + s.from
    db => db.createQuery(query).map(gr)
  }



  def makeHandle(conf: Configuration): Handle = {
    val dbi = if (conf.driver.equalsIgnoreCase("postgres")) {
      val source = new PGPoolingDataSource


      source.setDataSourceName(conf.name)
      source.setServerName(conf.server)
      source.setDatabaseName(conf.database)
      source.setUser(conf.user)
      source.setPassword(conf.password)
      source.setMaxConnections(10)
      new DBI(source)
    } else {
      val source = new MysqlDataSource
      //source.setDataSourceName(conf.name)
      source.setServerName(conf.server)
      source.setDatabaseName(conf.database)
      source.setUser(conf.user)
      source.setPassword(conf.password)
      //source.setMaxConnections(10)
      new DBI(source)
    }
    val gigmolHandle: Handle = dbi.open

    gigmolHandle
  }


  def uuid() = java.util.UUID.randomUUID.toString

  def main(args:Array[String]):Unit =
  {
    case class Woordje(lemma:String, pos:String, id:String)
    val exampleQuery =
      Select(
        mapping = r =>
          Woordje(r.getString("modern_lemma"), r.getString("lemma_part_of_speech"), r.getString("persistent_id")),
        from = "data.lemmata where lemma_part_of_speech ~ 'NOU'")


    //Hilex.stream(exampleQuery).filter(w => w.lemma.length > 3 && w.lemma.reverse == w.lemma).foreach(println)
  }
}

class Database(configuration: Configuration) {

  import DatabaseUtilities._
  import org.postgresql.copy.CopyManager
  import org.postgresql.ds._

  def getConfiguration = configuration

  def makeHandle(conf: Configuration): (Handle, javax.sql.DataSource) = {

    val (dbi,source) = if (conf.driver.equalsIgnoreCase("postgres")) {
      val source = new org.postgresql.ds.PGSimpleDataSource


      //source.setDataSourceName(conf.name)
      source.setServerName(conf.server)
      source.setDatabaseName(conf.database)
      source.setUser(conf.user)
      source.setPassword(conf.password)
      //source.setMaxConnections(10)
      (new DBI(source), source)
    } else {
      val source = new MysqlDataSource
      //source.setDataSourceName(conf.name)
      source.setServerName(conf.server)
      source.setDatabaseName(conf.database)
      source.setUser(conf.user)
      source.setPassword(conf.password)
      //source.setMaxConnections(10)
      (new DBI(source), source)
    }

    val gigmolHandle: Handle = dbi.open

    (gigmolHandle, source)
  }

  lazy val (handle, source) = makeHandle(configuration)

  def stream[A](a: AlmostQuery[A]): Stream[A] = DatabaseUtilities.stream(handle, a)

  def slurp[A](a: AlmostQuery[A]): List[A] = DatabaseUtilities.slurp(handle, a)
  def iterator[A](a: AlmostQuery[A]): Iterator[A] = DatabaseUtilities.iterator(handle, a)

  import scala.util.{Success,Failure,Try}

  def runStatement(statement: String): Unit =
  {
    Try(handle.execute(statement))
      match {
      case Success(_) => Console.err.println(s"Successfully executed $statement")
      case scala.util.Failure(e) => Console.err.println(s"Exception executing $statement: $e")}
  }

  def loadFile(tableName: String, file: java.io.File):Unit = {
    if (source.isInstanceOf[PGSimpleDataSource])
      {
        //handle.getConnection.getClientInfo.
        Console.err.println("Yoho, go for it!")
        val conn = source.getConnection
        val cm = new CopyManager(conn.asInstanceOf[BaseConnection])
        val iStream = new FileInputStream(file)
        cm.copyIn(s"COPY $tableName FROM STDIN", iStream)
        iStream.close
      } else { // dit werkt niet.......
      val fullPath = file.getCanonicalPath
      val statement = s"\\copy $tableName from '$fullPath' with delimiter E'\\t'"
      runStatement(statement)
    }
  }

  case class Binding(name: String, value: Any)

  case class QueryBatch[T](sql: String, f: T => Seq[Binding])
  {
    private lazy val batch = handle.prepareBatch(sql)
    def insert(l: Seq[T]):Unit =
    {
      l.foreach( t => {
        val b = f(t).foldLeft(batch)({ case (ba,bi) => ba.bind(bi.name, bi.value)})
        b.add()
      })
      batch.execute()
    }
  }

  def getAllFields(r: ResultSet):Map[String,String] =
  {
    val m = r.getMetaData
    (1 to m.getColumnCount).map(i => m.getColumnName(i) -> r.getString(i)).toMap
  }

  val getInMap = ResultMapping[Map[String,String]](getAllFields)

  def allRecords(tableName: String): AlmostQuery[Map[String,String]] = {
    db =>
      db.createQuery(
        s"""
      select * from $tableName""").map(getInMap)
  }

  def allFields(tableName: String):Set[String] = this.stream(this.allRecords(tableName)).head.keySet

  def replaceAll(tableName: String, pattern: String, replacement: String) =
  {
    val fields = this.allFields(tableName)
    fields.foreach(f => this.runStatement(s"update $tableName set $f='$replacement' where cast($f as text)='$pattern'"))
  }
}

object o extends Database(null)
