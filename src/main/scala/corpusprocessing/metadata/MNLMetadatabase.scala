package corpusprocessing.metadata

import corpusprocessing.onw
import database.{Configuration, Database}
import utils.ProcessFolder

import java.io.{File, PrintWriter}
import scala.xml.XML
import scala.xml._

object MNLMetadatabase {

  val c = new Configuration(name = "some_name", server = "svowdb20.ivdnt.loc", user = "postgres", password = "inl", database = "mnl_meta")
  val cHome = new Configuration(name = "some_name", server = "localhost", user = "postgres", password = "inl", database = "mnl_meta")
  val db = new Database(c)


  val metadataDirsX = List(onw.Settings.gysselingProcessedMetadataDir, onw.Settings.mnlProcessedMetadataDir)
  val ONWMetaDir = "/mnt/Projecten/Corpora/Historische_Corpora/ONW/ONW-processed-metadata-v2"
  val MNLMetaDir="/mnt/Projecten/corpora/Historische_Corpora/MNL-TEI/Nederlabversie/PostProcessedMetadata"
  val metadataDirs = List(MNLMetaDir)
  val dquote = "\""
  val escapedDquote = "\\\\'" // "\\\\\""

  def toArray(v: Seq[String]) = s"{$dquote${
    v.map(
      _.replaceAll(", +, *", ", ")
        .replaceAll(",", "\\\\,")
        .replaceAll("'", "\\\\'").replaceAll("\"", escapedDquote)).mkString(",")
  }$dquote}"

  case class Metadata(pid: String, data: Map[String, String]) {
    def asLine = pid + "\t" + allFields.map(f => {
      val v = this.data.getOrElse(f, "")
      v
    }).mkString("\t")
  }

  def getId(n: Node):Option[String] = n.attributes.filter(a => a.prefixedKey.endsWith(":id") || a.key=="id").map(_.value.toString).headOption


  def findOrigListBibl(d: Elem) = (d \\ "listBibl").filter(l => (getId(l).map(x => x.toLowerCase.contains("original-metadata-cdrom-version")
    && !x.contains("Level0_citaat-id")) == Some(true)))

  def getAllMetaFromDir(d: String) = {
    val data: Seq[Metadata] = ProcessFolder.processFolder(new java.io.File(d), f => {
      Console.err.println(f)
      val x = XML.loadFile(f)
      val apm = new addProcessedMetadataValues
      val b = apm.findListBibl(x)
      val b1 = findOrigListBibl(x)
      val data = (b \\ "interpGrp" ++ b1 \\ "interpGrp").map(ig => {
        val n = (ig \ "@type").text
        val v = (ig \ "interp").map(_.text.trim.replaceAll("\\s+", " ")).filter(_.trim.nonEmpty)
        n -> v
      }).toMap
        .map({case (k,v) if (k.contains("factu")) => normalizeFieldName(k) -> List("non-fiction"); case (k,v) => (normalizeFieldName(k),v)})
        .filter(_._2.exists(_.nonEmpty))
        .mapValues(_.mkString("‖"))

      // Some(data.filter(_._1.contains("oca"))).filter(_.nonEmpty).foreach(x => Console.err.println(x))
      val pid = data(normalizeFieldName("pid"))
      Metadata(pid, data.filter(_._2.nonEmpty))
    })
    data
  }

  def normalizeFieldName(f0: String) = {
    val f = f0.replaceAll("\\.","_")
    val f1 = if (f.matches("^[A-Z_]+$")) f.toLowerCase else f
    if (f1.contains("\"")) f1 else ("\"" + f1 + "\"")
  }

  lazy val allMetadata = metadataDirs.flatMap(getAllMetaFromDir)
  lazy val allFields = allMetadata.flatMap(_.data.keySet).toSet.toList.sorted.map(normalizeFieldName)
  lazy val addedFields = (addProcessedMetadataValues()).addedFieldNames.filter(allFields.contains(_)).map(normalizeFieldName) diff Seq("title")
  lazy val level1Fields = allFields.filter(_.contains("Level1")).map(normalizeFieldName)
  lazy val level2Fields = allFields.filter(_.contains("Level2")).map(normalizeFieldName)
  lazy val multiValuedFields = allMetadata.flatMap(m => m.data.filter(_._2.size > 1).map(_._1)).toSet
  lazy val singleValuedFields = (allFields.toSet diff multiValuedFields).map(normalizeFieldName)

  lazy val textsoortFields = allFields.filter(_.toLowerCase.matches(".*(fict|fact|genre|authen).*")).map(normalizeFieldName) diff addedFields
  lazy val dateFields = allFields.filter(_.toLowerCase.matches(".*(day|month|year).*")).map(normalizeFieldName) diff addedFields
  lazy val locationFields = allFields.filter(_.toLowerCase.matches(".*(country|place|region|kloeke).*")).map(normalizeFieldName) diff addedFields
  lazy val basicFields = allFields.filter(_.toLowerCase.matches(".*(author|title).*")).map(normalizeFieldName) diff addedFields
  lazy val edition_type_fields = basicFields ++ List("bron", "bron_opm")

  lazy val tsD = s"drop table if exists tekstsoort"
  lazy val tsQ = s"""create table tekstsoort as select "persistentId", title, ${textsoortFields.mkString(",")}  from metadata"""
  lazy val tsKey = """alter table tekstsoort add primary key ("persistentId")"""
  lazy val datesQ = s"""create view dates as select "persistentId", title, ${dateFields.mkString(",")}  from metadata"""
  lazy val locationsQ = s"""create view locations as select "persistentId", title, ${locationFields.mkString(",")}  from metadata"""
  lazy val basicQ = s"""create view basic as select "persistentId", ${basicFields.mkString(",")}  from metadata"""
  lazy val edition_typeQ = s"""create table edition_type as select "persistentId", ${edition_type_fields.mkString(",")}  from metadata"""
  lazy val extraatjes = List("is_critical" -> "boolean", "comment" -> "text").map(f => s"alter table edition_type add column ${f._1} ${f._2}") ++ List("update edition_type set is_critical=true")

  def makeCreate(name: String, fields: List[String]) =
    s"""
       |create table $name (
       |"persistentId" text primary key,
       |${fields.map(f => s"$f text").mkString(", ")}
       |)
       |""".stripMargin

  def makeSelect(name: String, fields: List[String]) =
    s"""
       |create view $name
       |as select "persistentId", title,
       |${fields.map(f => s"$f").mkString(", ")}
       | from metadata
       |""".stripMargin

  lazy val createStatement =
    s"""
       |create table metadata (
       |"persistentId" text primary key,
       |${allFields.map(f => s"$f text").mkString(", ")}
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

    (List(tsD, tsQ, tsKey, basicQ, locationsQ, datesQ, edition_typeQ) ++ extraatjes).foreach(db.runStatement(_))
  }
}
