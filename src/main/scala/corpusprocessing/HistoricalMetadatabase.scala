package corpusprocessing
import java.io.{File, PrintWriter}

import database._
import utils.ProcessFolder

import scala.xml._

object HistoricalMetadatabase {
   val c = new Configuration(name="some_name", server = "svowdb20.ivdnt.loc", user = "postgres", password="inl", database = "historical_meta")
   val db = new Database(c)


   val metadataDirs = List(onw.Settings.gysselingProcessedMetadataDir, onw.Settings.mnlProcessedMetadataDir)

  val dquote = "\""
  val escapedDquote =  "\\\\'" // "\\\\\""

  def toArray(v: Seq[String]) = s"{$dquote${v.map(
      _.replaceAll(", +, *", ", ")
      .replaceAll(",", "\\\\,")
      .replaceAll("'", "\\\\'").replaceAll("\"", escapedDquote)).mkString(",")}$dquote}"

   case class Metadata(pid: String, data: Map[String, Seq[String]]) {
     def asLine = pid + "\t" + allFields.map(f => {
       val v = this.data.getOrElse(f, Seq())
       toArray(v)
     }).mkString("\t")
   }

   def getAllMetaFromDir(d: String) = {
     val data: Seq[Metadata] = ProcessFolder.processFolder(new java.io.File(d), f => {
       val x = XML.loadFile(f)
       val apm = new addProcessedMetadataValues
       val b = apm.findListBibl(x)
       val data = (b \\ "interpGrp").map(ig => {
         val n = (ig \ "@type").text
         val v = (ig \ "interp").map( _.text.trim.replaceAll("\\s+", " ")).filter(_.nonEmpty)
         n -> v
       }).toMap
       val pid = data("pid").head
       Metadata(pid, data.filter(_._2.nonEmpty))
     })
     data
   }


  lazy val allMetadata = metadataDirs.flatMap(getAllMetaFromDir)
  lazy val allFields = allMetadata.flatMap(_.data.keySet).toSet.toList.sorted
  lazy val addedFields = (addProcessedMetadataValues()).addedFieldNames.filter(allFields.contains(_)).map(_.toLowerCase) diff Seq("title")
  lazy val level1Fields = allFields.filter(_.contains("Level1")).map(_.toLowerCase)
  lazy val level2Fields = allFields.filter(_.contains("Level2")).map(_.toLowerCase)
  lazy val multiValuedFields = allMetadata.flatMap(m => m.data.filter(_._2.size > 1).map(_._1)).toSet
  lazy val singleValuedFields = (allFields.toSet diff multiValuedFields).map(_.toLowerCase)

  lazy val textsoortFields = allFields.filter(_.toLowerCase.matches(".*(fict|fact|genre|authen).*")).map(_.toLowerCase) diff addedFields
  lazy val dateFields = allFields.filter(_.toLowerCase.matches(".*(day|month|year).*")).map(_.toLowerCase) diff addedFields
  lazy val locationFields = allFields.filter(_.toLowerCase.matches(".*(country|place|region|kloeke).*")).map(_.toLowerCase) diff addedFields
  lazy val basicFields = allFields.filter(_.toLowerCase.matches(".*(author|title).*")).map(_.toLowerCase) diff addedFields

  lazy val tsQ = s"create view textsoort as select persistentid, title, ${textsoortFields.mkString(",")}  from metadata"
  lazy val datesQ = s"create view dates as select persistentid, title, ${dateFields.mkString(",")}  from metadata"
  lazy val locationsQ = s"create view locations as select persistentid, title, ${locationFields.mkString(",")}  from metadata"
  lazy val basicQ = s"create view basic as select persistentid, ${basicFields.mkString(",")}  from metadata"


  def makeCreate(name: String, fields: List[String]) =
    s"""
       |create table $name (
       |persistentId text primary key,
       |${fields.map(f => s"$f text").mkString(", ")}
       |)
       |""".stripMargin

  def makeSelect(name: String, fields: List[String]) =
    s"""
       |create view $name
       |as select persistentId, title,
       |${fields.map(f => s"$f").mkString(", ")}
       | from metadata
       |""".stripMargin

  lazy val createStatement =
    s"""
       |create table metadata (
       |persistentId text primary key,
       |${allFields.map(f => s"$f text[]").mkString(", ")}
       |)
       |""".stripMargin



  def main(args: Array[String]): Unit = {
    //allMetadata.foreach(println)

    db.runStatement("drop table if exists metadata cascade")
    println(createStatement)
    db.runStatement(createStatement)

    //db.runStatement(makeCreate("level1", level1Fields))


    val p = new PrintWriter("/tmp/data.out")

    allMetadata.foreach(m => p.println(m.asLine))
    p.close()
    db.loadFile("metadata", new File("/tmp/data.out"))

    singleValuedFields.foreach(f => db.runStatement(s"alter table metadata alter column $f set data type text using $f[1];"))
    addedFields.foreach(f => db.runStatement(s"alter table metadata drop column $f"))


    db.runStatement(makeSelect("level1", level1Fields))
    db.runStatement(makeSelect("level2", level2Fields))

    List(tsQ,basicQ,locationsQ,datesQ).foreach(db.runStatement(_))
  }
}
