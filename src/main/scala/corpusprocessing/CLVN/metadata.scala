package corpusprocessing.CLVN

import java.sql.ResultSet

import database.DatabaseUtilities.{AlmostQuery, ResultMapping}

import scala.xml._

class metabase(configuration: database.Configuration) extends database.Database(configuration)

case class FieldMapping(fieldName: String, f: Bron => String)


trait BronType
object deelBronType {

  case object textBron extends BronType
  case object milestoneBron extends BronType
  case object divBron extends BronType
  case object weetNogNiet extends BronType
  val daysOfWeek = Seq(textBron, milestoneBron, divBron)
}
import deelBronType._

case class Bron(fields: Map[String,String], fieldMap: List[FieldMapping], deelBronnen: List[Bron] = List(), bronType: BronType=textBron,  finaal:Boolean = false)
{
  import deelBronType._

  import metadata.simpleFieldMapping

  def maakErPidVan(s:String): String = if (s == "idno" && finaal) "pid" else s

  def pruttel(n:String,v:String):NodeSeq =
    {
      //Console.err.println(s"$n=$v")
      if (v != null && v.nonEmpty) <interpGrp
      inst={if (bronType == divBron) inst.map(i => Text("#" + i.toString)) else None}
      type={maakErPidVan(veldenLijst.camelize(n))}>{v.split("\\s*\\|\\s*")
        .map(fv => <interp>{fv}</interp>)}</interpGrp> else Seq()
    }

  def part_key: Option[String] = if (bronType == textBron || !fields.get("parent_id").isDefined || !fields.get("id_text_part").isDefined) None
  else Some(fields("parent_id") + "." + fields("id_text_part"))

  // if (bronType != textBron) Console.err.println(s"$bronType : $part_key")

  lazy val xtra : List[FieldMapping]= if (bronType != textBron) List(("sourceID", "id_text_part")) else List()

  lazy val mappedFields:List[(String,String)] = if (fieldMap.isEmpty) fields.toList.filter(x  => !veldenLijst.anatesAlieni.contains(x._1)) else
    (fieldMap.filter(fm => !xtra.exists(f => f.fieldName == fm.fieldName))  ++ xtra).map(
    {case FieldMapping(field, f) => field -> f(this)}).filter(x  => !veldenLijst.anatesAlieni.contains(x._1))

  lazy val mappedFieldMap:Map[String,String] = mappedFields.map({case (x,y) => (x.toLowerCase, y)}).toMap

  def mappedField(f: String)  = {
    if (!mappedFieldMap.contains(f.toLowerCase()))
      {
        Console.err.println(s"$f NOT IN FIELDMAP: $mappedFieldMap\n$fields")
      }
    mappedFieldMap(f.toLowerCase())
  }

  lazy val inst:Option[Text] = if (bronType==divBron || bronType==milestoneBron) Some(Text("INT_" + fields("id_text_part"))) else None

  lazy val biblId = if (!finaal || true) inst.map(t => Text("bibl_" + t.toString())) else Some(Text("bibl_" + mappedFieldMap("idno")))

  def filterField(f: (String,String)) = if (!finaal || bronType == textBron) true else {
    f._1.toLowerCase.contains("level0")  || Set("idno", "pid").contains(f._1)
  }

  lazy val bibl:Elem = // moet nog een id krijgen, en de divs en milestones die corresponderen moeten ana hebben?
    <bibl type={bronType.toString} xml:id={biblId}>
      {mappedFields.filter(filterField).map({case (n,v) => pruttel(n,v)})}
      <biblScope>
      {if (bronType==milestoneBron)
       <span from={"#INT_" + fields("id_text_part")  + "-b"} to={"#INT_" + fields("id_text_part")  + "-e"}/>
        else if (bronType == divBron)
       <span inst={"#INT_" + fields("id_text_part")}/> // dit is redundant
      }
      </biblScope>
    </bibl>


  lazy val header: Elem =
    <teiHeader>
      <fileDesc>
        <titleStmt>
          <title>{mappedField("titleLevel1")}</title>
        </titleStmt>
        <publicationStmt>
          <p>
            <date></date>
            <idno type="sourceID">{mappedField("sourceID")}</idno>
            <idno type="pid">{mappedField("idno")}</idno>
          </p>
        </publicationStmt>
        <notesStmt>
          <note/>
        </notesStmt>
        <sourceDesc>
          <listBibl xml:id="inlMetadata">
            {bibl}
            {deelBronnen.map(_.bibl)}
          </listBibl>
        </sourceDesc>
      </fileDesc>
    </teiHeader>

  // dit was niet goed, alleen de globale metadata moet in de bovenste listBibl....
}

object metadata {

  val useMapping:Boolean = false

  def cleanId(id: String):String = {
    if (id == null) return ""
    val after = id.trim.replaceAll("[^A-Za-z0-9_.-]","_");  after }

  def transcribent(b:Bron):String =
  {
    val x = b.fields("transcriptor").trim
    if (x.isEmpty) b.fields("aanv_trans") else x
  }

  implicit def simpleFieldMapping(p: (String,String)):FieldMapping = FieldMapping(p._1, b => b.fields(p._2))

  def periodStart(p: String):String = { val c = p.split("\\s*[-/]\\s*"); if (c.size<=1) p else c(0)}
  def periodEnd(p: String):String = { val c = p.split("\\s*[-/]\\s*"); if (c.size<=1) p else c(1)}

  def xuuid(b: Bron):String =
  {
    val m:Map[String,String] = b.fields
    val source = cleanId(if (b.bronType == milestoneBron) m("id_text_part") else m("record_id"))
    val bytes = source.getBytes("UTF-8")
    java.util.UUID.nameUUIDFromBytes(bytes).toString
  }

  val minimalMapping: List[FieldMapping] = List(
    //FieldMapping("idno", uuid),
    ("sourceID","record_id"),
    FieldMapping("titleLevel1", m => "unknown")
  )

  val leuvenseMapping:List[FieldMapping] = List(
    // FieldMapping("idno", uuid),
    ("sourceID","record_id"),
    ("titleLevel1", "titel_document"),
    ("authorLevel1", "auteur"),
    ("witnessDay_from", "day"),
    ("witnessMonth_from", "month"),
    ("witnessDay_to", "day"),
    ("witnessMonth_to", "month"),
    ("localization_place", "plaats"),
    ("localization_province", "provincie"),
    ("genre", "genre"),
    ("biblscope_page", "aanv_folio"),
    FieldMapping("editorLevel1", transcribent),
    FieldMapping("witnessYear_from", b => periodStart(b.fields("periode_of_jaar"))),
    FieldMapping("witnessYear_to", b => periodEnd(b.fields("periode_of_jaar")))
  )

  val ditMapping:List[FieldMapping] = List(
    //FieldMapping("idno", uuid),
    ("sourceID","referentiecode"),
    ("titleLevel1", "titel_document"),
    /*
    ("authorLevel1", "auteur"),
    ("witnessDay_from", "day"),
    ("witnessMonth_from", "month"),
    ("witnessDay_to", "day"),
    ("witnessMonth_to", "month"),
    */
    ("localization_place", "plaats"),
    ("localization_province", "provincie"),
    ("genre", "genre"),
    //("biblscope_page", "aanv_folio"),
    //FieldMapping("editorLevel1", transcribent),
    FieldMapping("witnessYear_from", b => periodStart(b.fields("periode"))),
    FieldMapping("witnessYear_to", b => periodEnd(b.fields("periode")))
  )



  lazy val ditBase = new metabase(Settings.ditconfig)

  def getAlleBronnen(tableName: String, mappings: List[FieldMapping]): Map[String, Bron] = {
    val tableNamex = if (useMapping) tableName else "cleaned_metadata"
    val mappingsx = if (useMapping) mappings else List.empty[FieldMapping]
    ditBase.slurp(ditBase.allRecords(tableNamex)).map(m => Bron(m, mappingsx)).groupBy(b => cleanId(b.fields("record_id"))).mapValues(_.head)
  }

  def getDeelBronnenByPartKey(tableName: String, mappings: List[FieldMapping]): Map[String, Bron] = {
    val tableNamex = if (useMapping) tableName else "cleaned_metadata"
    val mappingsx = if (useMapping) mappings else List.empty[FieldMapping]
    val alles = ditBase.slurp(ditBase.allRecords(tableNamex))
      .map(m => Bron(m, mappingsx, bronType = weetNogNiet))
      .filter(_.part_key.isDefined)
      .groupBy(b => cleanId(b.part_key.get))
      .mapValues(_.head)
    // Console.err.println("Deelbronnen voor $tableName: " + alles)
    alles
  }

  lazy val leuvenseBronnen: Map[String, Bron] = getAlleBronnen("leuven", leuvenseMapping)
  lazy val ditBronnen: Map[String, Bron] = getAlleBronnen("dit", ditMapping)

  lazy val ditByReference: Map[String,Bron] = ditBronnen.values
    .groupBy(_.fields(if (useMapping) "referentiecode" else "record_id")).mapValues(_.head)
}
