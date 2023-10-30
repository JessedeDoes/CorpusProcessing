package corpusprocessing.CLVN

import database.DatabaseUtilities._
import enhanceMetadata.{FieldMapping, periodEnd, periodStart, plainDating, simpleFieldMapping, transcribent, uuid}

import scala.util.{Failure, Success, Try}
import scala.util.matching.Regex

// !! Niedorp is een puinhoop vanwege dubbele veldnamen....
// handmatig omzetten naar veldnamen als graft en

object enhanceMetadata {
  val ditconfig = database.Configuration(
    name = "ditmeta",
    server = "svowdb02",
    database = "ditmetadata",
    user = "postgres",
    password = "inl")

  lazy val ditBase = new metabase(ditconfig)

  object mappings {
    import enhanceMetadata.simpleFieldMapping
    import enhanceMetadata.Bron

    implicit def simpleFieldMapping(p: (String,String)):enhanceMetadata.FieldMapping = FieldMapping(p._1, b => b(p._2))

    val basicMapping: List[enhanceMetadata.FieldMapping] = List(

      FieldMapping("idno", uuid),
      ("collectie", "collectie"),
      ("filenaam", "filenaam"),
      ("record_id", "record_id"),
      ("id_text_part", "id_text_part"),
      ("parent_id", "parent_id"), // nog toevoegen in leuven en dit tabellen

      ("sourceID", "record_id"),
      ("titleLevel1", "titel_document"),
      ("authorLevel1", "auteur"),

      ("titleLevel2", "bron"),

      FieldMapping("witnessDayLevel1_from", b => plainDating(b)._2.toString),
      FieldMapping("witnessMonthLevel1_from", b => plainDating(b)._1.toString),



      FieldMapping("witnessYearLevel1_from", b => periodStart(b("periode_of_jaar"))),
      FieldMapping("witnessYearLevel1_to", b => periodEnd(b("periode_of_jaar"))),

      ("localization_placeLevel1", "plaats"),
      ("localization_provinceLevel1", "provincie"),

      FieldMapping("genreLevel1", b => b("genre").toLowerCase),
      FieldMapping("subgenreLevel1", b => b("subgenre").toLowerCase),
      ("biblscope_pageLevel1", "aanv_folio"),

      FieldMapping("editorLevel1", transcribent)
    )

    val dateringVelden = Set("witnessDayLevel1_from", "witnessMonthLevel1_from", "witnessYearLevel1_from", "witnessYearLevel1_to")
    val lokaliseringVelden = Set("localization_placeLevel1", "localization_provinceLevel1")

    val ditDateringen: List[enhanceMetadata.FieldMapping] = List(
      FieldMapping("witnessYearLevel1_from", b => periodStart(b("periode"))),
      FieldMapping("witnessYearLevel1_to", b => periodEnd(b("periode")))
    )

    val ditMapping = basicMapping.filter(x => !dateringVelden.contains(x.fieldName)) ++ ditDateringen

    val gemertErfpachtDateringen: List[enhanceMetadata.FieldMapping] = List(
      FieldMapping("witnessYearLevel1_from", b => periodStart(b("datering_tekstgetuige_jaar"))),
      FieldMapping("witnessYearLevel1_to", b => periodEnd(b("datering_tekstgetuige_jaar"))),
      ("textYearLevel1_to", "tekst_jaar"),
      ("textYearLevel1_from", "tekst_jaar"),
      FieldMapping("textDayLevel1_from", b => plainDating(b, "tekst_dag")._2.toString),
      FieldMapping("textMonthLevel1_from", b => plainDating(b, "tekst_dag")._1.toString)
    )

    val gemert_erfpacht_mapping = basicMapping.filter(x => !dateringVelden.contains(x.fieldName)) ++ gemertErfpachtDateringen

    /*
  Gemert erfpacht:
  datering tekstgetuige jaar	tekst-jaar	tekst-dag
  1500-1600	circa 1500
  1500-1600	1477	25-6-1477

  Alkmaar 1549:
  Titel document	DATERING TG	DAG tg	DATERING TEKST	Dag dt
  Resoluties Alkmaarse Vroedschappen	1549-1565		1549-1565

   */

    val alkmaar1549Dateringen: List[enhanceMetadata.FieldMapping] = List(
      FieldMapping("witnessYearLevel1_from", b => periodStart(b("datering_tg"))),
      FieldMapping("witnessYearLevel1_to", b => periodEnd(b("datering_tg"))),
      FieldMapping("textYearLevel1_from", b => periodStart(b("datering_tekst"))),
      FieldMapping("textYearLevel1_to", b => periodEnd(b("datering_tekst"))),
      FieldMapping("textDayLevel1_from", b => plainDating(b, "dag_dt")._2.toString),
      FieldMapping("textMonthLevel1_from", b => plainDating(b, "dag_dt")._1.toString) // witnessDay etc niet ingevuld
    )

    val alkmaar1549Mapping = basicMapping.filter(x => !dateringVelden.contains(x.fieldName)) ++ alkmaar1549Dateringen


    // de datering van het hele speciaaldocument komt nog niet mee

    val alkmaarSpeciaalDateringen: List[enhanceMetadata.FieldMapping] = List(
      FieldMapping("witnessYearLevel1_from", b => periodStart(b("periode_of_jaar"))),
      FieldMapping("witnessYearLevel1_to", b => periodEnd(b("periode_of_jaar"))),

      FieldMapping("witnessDayLevel1_from", b => b("day")),
      FieldMapping("witnessMonthLevel1_from", b => b("month"))
    )

    val alkmaarSpeciaalMapping = basicMapping.filter(x => !dateringVelden.contains(x.fieldName)) ++ alkmaarSpeciaalDateringen


/*
    DTG Periode of jaar	DTG Dag	DTG Plaats	DTG Provincie	DT Periode of jaar	DT Dag	LT Plaats	LT Provincie
      1571	30-6-1571	Alkmaar	Noord-Holland	1571	30-6-1571	Alkmaar	Noord-Holland

      Graft lijkt zich net zo te gedragen

*/
    val alkmaarBelegstukken1571DateringenenLokaliseringen: List[FieldMapping]  = List(
      FieldMapping("witnessYearLevel1_from", b => periodStart(b("dtg_periode_of_jaar"))),
      FieldMapping("witnessYearLevel1_to", b => periodEnd(b("dtg_periode_of_jaar"))),

      FieldMapping("textYearLevel1_from", b => periodStart(b("dt_periode_of_jaar"))),
      FieldMapping("textYearLevel1_to", b => periodEnd(b("dt_periode_of_jaar"))),

      FieldMapping("witnessDayLevel1_from", b => plainDating(b, "dtg_dag")._2.toString),
      FieldMapping("witnessMonthLevel1_from", b => plainDating(b, "dtg_dag")._1.toString),

      FieldMapping("textDayLevel1_from", b => plainDating(b, "dt_dag")._2.toString),
      FieldMapping("textMonthLevel1_from", b => plainDating(b, "dt_dag")._1.toString),

      ("localization_placeLevel1", "dtg_plaats"),
      ("localization_provinceLevel1", "dtg_provincie"),

      ("textLocalization_placeLevel1", "lt_plaats"),
      ("textLocalization_provinceLevel1", "lt_provincie")
    )

    val alkmaarBelegstukkenMapping = basicMapping.filter(x => !(dateringVelden ++ lokaliseringVelden).contains(x.fieldName)) ++ alkmaarBelegstukken1571DateringenenLokaliseringen
  }

  class Sheet(_records: List[Bron])
  {
    def records = _records

    lazy val fileName:String = records.head("filenaam").replaceAll(".csv$", "")
    lazy val collectie:String = records.head("collectie")

    override def toString = s"#####$collectie: $fileName bronnen:${records.size} waarvan textdelen: ${records.count(b => b.isDeelbron())}" //  ${records.map(_.toString).mkString("\n")}"

    def transform(m: Bron):Bron = ???

    def transformedSheet():Sheet = new Sheet(records.map(transform))
  }

  type string2string = String=>String

  trait Bron extends string2string
  {
    def domain:Set[String]

    override def toString = domain.toList.sorted.filter(this(_) != "").map(s => s"$s => ${this(s)}").mkString(" ")

    def hasField(f: String):Boolean = domain.contains(f) &&
      this(f) != null && this(f).trim != ""

    def isSimpeleBron():Boolean = hasField("record_id")
    def isDeelbron():Boolean = hasField("id_text_part")
  }

  class MapBron(m: Map[String,String]) extends Bron
  {
    def apply(s: String)  = m(s)
    def domain:Set[String] = m.keySet
  }

  implicit def asBron(m: Map[String,String]):Bron  = new MapBron(m)

  case class MappedBron(m: Bron, mapping: List[FieldMapping]) extends Bron
  {
    lazy val domain: Set[String] = mapping.map(_.fieldName).toSet

    def apply(s: String):String = if (domain.contains(s))
      mapping.filter(_.fieldName ==s).head.f(this)
    else ""
  }

  case class FieldMapping(fieldName: String, f: Bron => String)

  implicit def simpleFieldMapping(p: (String,String)):FieldMapping = FieldMapping(p._1, b => b(p._2))

  case class SimpleSheet(_records: List[Bron], mapping: List[FieldMapping]) extends Sheet(_records)
  {
    override def transform(b: Bron):Bron =
    {
      val newBronData:Map[String,String] = mapping.map({case FieldMapping(field, f) => field -> f.apply(b)}).toMap
      new MapBron(newBronData)
    }
  }

  def periodStart(p: String):String = { val c = p.split("\\s*[-/]\\s*"); if (c.size<=1) p else c(0)}
  def periodEnd(p: String):String = { val c = p.split("\\s*[-/]\\s*"); if (c.size<=1) p else c(1)}

  def uuid(b: Bron):String =
  {
    val source = metadata.cleanId(b("id_text_part")  +  b("record_id"))
    val bytes = source.getBytes("UTF-8")
    java.util.UUID.nameUUIDFromBytes(bytes).toString
  }

  def transcribent(b:Bron):String =
  {
    val x = b("transcriptor").trim
    if (x.isEmpty) b("aanv_trans") else x
  }

  /*
     De meerderheid van de sheets heeft deze structuur:
   */




  val maanden = List(
    "jan|januari|january",
    "feb|februari|february",
    "mrt|mar|maart|march",
    "apr|april",
    "mei|may",
    "jun|juni|june",
    "jul|juli|july",
    "aug|august|augustus",
    "sep|september",
    "okt|oct|oktober|october",
    "nov|november",
    "dec|december")

  // resource linkjes iets als http://www.itineranova.be/in/SAL7352/R%C2%B0144.2/act

  val drieGetallen:Regex = "([0-9]+)\\s*[/-]\\s*([0-9]+)\\s*[/-]\\s*([0-9]+)".r
  val tweeGetallenMetJaarAlsLaatste:Regex = "([0-9]+)\\s*[/-]\\s*([0-9]{4})".r
  val tweeGetallen:Regex = "([0-9]+)\\s*[/-]\\s*([0-9]+)".r
  val maandGetal:Regex = s"""(?i)(${maanden.mkString("|")})(\\s|-)*([0-9]+)""".r
  val getalMaand:Regex = s"(?i)([0-9]{1,2})\\s*[/-]?\\s*(${maanden.mkString("|")})".r
  val alleenMaarEenMaand:Regex = s"(?i)\\s*(${maanden.mkString("|")})-?x*".r

  def tryOption[S,T](x: Option[S], f: S => T):Option[T] = Try(f(x.get)) match { case Success(y) => Some(y); case Failure(e) => None}

  def parseDay(month_day: String):(Option[Int], Option[Int], String, String) =
  {
    val dl = month_day.toLowerCase

    val month:Option[String] =
      if (maandGetal.findFirstMatchIn(dl).isDefined)
        maanden.zipWithIndex
          .find({case (m,i) => m.split("\\|").exists(x => dl.contains(x)) } )
          .map({case (m,i) => (i+1).toString})
      else if (drieGetallen.findFirstMatchIn(dl).isDefined) drieGetallen.findFirstMatchIn(dl).map(_.group(2))
      else if (tweeGetallenMetJaarAlsLaatste.findFirstMatchIn(dl).isDefined) tweeGetallenMetJaarAlsLaatste.findFirstMatchIn(dl).map(_.group(1))
      else if (tweeGetallen.findFirstMatchIn(dl).isDefined) tweeGetallen.findFirstMatchIn(dl).map(_.group(1))
      else if (getalMaand.findFirstMatchIn(dl).isDefined)
        maanden.zipWithIndex
          .find({case (m,i) => m.split("\\|").exists(x => dl.contains(x)) } )
          .map({case (m,i) => (i+1).toString})
      else if (alleenMaarEenMaand.findFirstMatchIn(dl).isDefined)
        maanden.zipWithIndex
          .find({case (m,i) => m.split("\\|").exists(x => dl.contains(x)) } )
          .map({case (m,i) => (i+1).toString})
      else None

    val realDay = if (maandGetal.findFirstMatchIn(dl).isDefined) maandGetal.findFirstMatchIn(dl).map(_.group(3))
    else if  (drieGetallen.findFirstMatchIn(dl).isDefined) drieGetallen.findFirstMatchIn(dl).map(_.group(1))
    else if (tweeGetallenMetJaarAlsLaatste.findFirstMatchIn(dl).isDefined) None
    else if (tweeGetallen.findFirstMatchIn(dl).isDefined) tweeGetallen.findFirstMatchIn(dl).map(_.group(2))
    else if (getalMaand.findFirstMatchIn(dl).isDefined) { val x = getalMaand.findFirstMatchIn(dl).map(_.group(1)); x }
    else { None }

    (tryOption[String,Int](month, x => Integer.parseInt(x)), tryOption[String,Int](realDay, x => Integer.parseInt(x)), month.getOrElse(s"NoMonth($month_day)"),
      realDay.getOrElse(s"NoDay($month_day)"))
  }

  /*
   jaar uit periode_of_jaar; maand en dag uit "dag"
   */

  case class DateStuff()
  lazy val alleBronnenBehalveLeuven:List[Map[String,String]] = ditBase.slurp(ditBase.allRecords("meta  " +
    "where not (collectie='10_Leuven' or filenaam ~* 'qh|graft|niedorp') order by collectie, record_id")) // maar anders voor leuven, etc...
  lazy val leuven:List[Map[String,String]] = ditBase.slurp(ditBase.allRecords("leuven order by collectie, record_id"))
  lazy val ditXML:List[Map[String,String]] = ditBase.slurp(ditBase.allRecords("dit where collectie='20_dit' order by collectie, record_id"))

  lazy val alkmaarPatches:List[Map[String,String]] = ditBase.slurp(ditBase.allRecords("meta_patch where filenaam ~* 'graft|niedorp' order by collectie, record_id"))

  lazy val alleBronnen:List[Map[String,String]] = alleBronnenBehalveLeuven ++ leuven ++ ditXML ++ alkmaarPatches

  case class Date(day: Option[Int], month: Option[Int], year: Option[Int])
  {

  }

 // 02_Archief 1564Elburg=Elburg 1564: mrt -->  None / None / Some(1564)
  // 04_ArchiefEnschede 1581bEnschede=Enschede 1581-05-06: 6-May -->  Some(6) / Some(5) / None
  def plainDating(m: Bron, f:String = "dag"):(String, String) =
  {
     val (month,day, mString, dayString) = parseDay(m(f))
     //val year = Try (m("periode_of_jaar").toInt) match { case Success(x) => Some(x); case Failure(e) => None }
     // println(s"${m("collectie")} ${m("record_id")}= ||${m("titel_document")}||: ${m("dag")} -->  $day / $month / $year")
     (mString,dayString)
  }

  def plainDatingApplies(m: Bron): Boolean =
  {
    val c = m("collectie")
    val f = m("filenaam")
    plainDatingApplies(c,f)
  }

  def plainDatingApplies(c: String, f: String): Boolean =
  {
    val forbidden = List(//"vredeboek",
      "qh",
      "erfpachtregister",
      "03_ArchiefAlkmaar_Jesse_Alkmaar_metadata_1549-1565_S.K.-WR".toLowerCase(),
      "speciaal", // deze moet nog opgelost. Geen dateringen in excel gevonden ? wel in database day, month, periode_of_jaar
      // "03_ArchiefAlkmaar_Jesse_Alkmaar_resoluties-vroedschap_1571-1575_S.K".toLowerCase, // deze is toch maar "gewoon?"
      "03_ArchiefAlkmaar_Jesse_Belegstukken-Stadsarchief-Alkmaar_1571-1578_TvS-WR".toLowerCase,
      "noorderkwartier")
    return c.matches("^(01|02|03|04|05|06|07|08|09|10|12|20).*") &&
      ! forbidden.exists(f.toLowerCase.contains(_))
  }

  def plainDatingApplies(s: Sheet):Boolean = plainDatingApplies(s.records.head)

  def isLeuven(m: Map[String,String]): Boolean = m("collectie") == "10_Leuven"

  // lazy val moreDifficultMainTexts = allDates.filter(m => !plainDatingApplies(m) && m("record_id") != "" && !isLeuven(m)).map(m => m("filenaam")).toSet
  // lazy val subtexts = allDates.filter(m => m("record_id") == "").map(m => (m("filenaam"), m("id_text_part"))).toSet

  lazy val sheetMap:Map[String, Sheet] = alleBronnen.groupBy(m => m("filenaam")).mapValues(x => new Sheet(x.filter(isDecentRecord).map(asBron)))

  lazy val allSheets:List[Sheet] = sheetMap.values.toList.sortBy(s => s.toString)

  def hasField(m: Map[String,String], f: String):Boolean = m.contains(f) && m(f) != null && m(f).trim != ""

  def isDecentRecord(m:Map[String,String]):Boolean = hasField(m,"record_id") || hasField(m, "id_text_part")

  lazy val simpleSheets:List[Sheet] = allSheets.filter(plainDatingApplies).map(s => SimpleSheet(s.records, mappings.basicMapping))

  lazy val gemertErfpachtSheets:List[Sheet] = allSheets
    .filter(s => s.fileName.contains("Gemert_Erfpacht"))
    .map(s => SimpleSheet(s.records, mappings.gemert_erfpacht_mapping))

  lazy val alkmaar1549Sheets: List[Sheet] = allSheets
    .filter(s => s.fileName.contains("03_ArchiefAlkmaar_Jesse_Alkmaar_metadata_1549-1565_S.K.-WR"))
    .map(s => SimpleSheet(s.records, mappings.alkmaar1549Mapping))

  lazy val alkmaarSpeciaalSheets: List[Sheet] = allSheets
    .filter(s => s.fileName.contains("eciaal"))
    .map(s => SimpleSheet(s.records, mappings.alkmaarSpeciaalMapping))

  lazy val alkmaarBelegSheets: List[Sheet] = allSheets
    .filter(s => s.fileName.contains("elegstu") || s.fileName.contains("Noorderkwartier_Graft") || s.fileName.contains("Noorderkwartier_de_Niedorp"))
    .map(s => SimpleSheet(s.records, mappings.alkmaarBelegstukkenMapping))

  lazy val ditSheets: List[Sheet] = allSheets
    .filter(s => s.fileName.toLowerCase.contains("qh"))
    .map(s => SimpleSheet(s.records, mappings.ditMapping))


  def batchInsert(s1: List[Bron]):Unit =
  {
    val allFields:List[String] = s1.flatMap(b => b.domain).distinct.sorted

    def bindings(b: Bron) = allFields.map(f => ditBase.Binding(f, if (b.domain.contains(f)) b(f) else "" )).toList

    val insertStatement = s"insert into cleaned_metadata (${allFields.mkString(",")}) values (${allFields.map(":" + _).mkString(",")})"

    Console.err.println(insertStatement)
    val batch = ditBase.QueryBatch(insertStatement, bindings)
    batch.insert(s1)
  }

  def aggregateDateToLevel2(level1Field: String) =
  {
    val level2Field = level1Field.replace("1", "2")
    val aggregator = if (level1Field.contains("from")) "min"  else "max"

    val q0 = "drop table if exists aggregates"

    val q1 = s"""create temporary table aggregates
             as select filenaam, cast(  $aggregator(cast($level1Field as integer)) as text) as aggie from cleaned_metadata
             where $level1Field ~ '^[0-9]+$$' group by filenaam
                 """

    val q =
      s"""
         |update cleaned_metadata set $level2Field = aggie from aggregates where aggregates.filenaam = cleaned_metadata.filenaam and $level2Field=''
         |or $level2Field is null
       """.stripMargin

    List(q0,q1,q).foreach(ditBase.runStatement(_))
  }

  def createUnifiedMetadataTable(): Unit =
  {
    val s1 = (
        simpleSheets ++ ditSheets
        ++ gemertErfpachtSheets
        ++ alkmaar1549Sheets
        ++ alkmaarSpeciaalSheets
        ++ alkmaarBelegSheets).map(s => s.transformedSheet())

    Console.err.println(s"Sheets: ${s1.size}")
    s1.sortBy(_.fileName).foreach(println)

    // ditBase.runStatement("delete from meta where collectie='09_HofboekBredevoort' and ")
    ditBase.runStatement("drop table if exists cleaned_metadata")

    val allFields:List[String] = s1.flatMap(s => s.records.flatMap(b => b.domain)).distinct.sorted

    val slechteVelden = allFields.filter( !veldenLijst.fieldOK(_) )
    if (slechteVelden.nonEmpty) {
      Console.err.println("Vervelende velden: " + slechteVelden)
      System.exit(1)
    }
    val allFieldsEnhanced = (allFields.toSet ++ veldenLijst.allFields).map(_.toLowerCase).toList.sorted

    val createTable = s"""create table cleaned_metadata (tellertje serial primary key, ${allFieldsEnhanced.map(f => s"$f text").mkString(",\n")})"""

    //Console.err.println(createTable)

    ditBase.runStatement(createTable)

    def bindings(b: Bron) = allFields.map(f => ditBase.Binding(f, if (b.domain.contains(f)) b(f) else "" )).toList

    def simpeleBronnen = s1.flatMap(_.records).filter(b => b("record_id") != "")
    def deelBronnen = s1.flatMap(_.records).filter(b => b.hasField("id_text_part"))

    batchInsert(simpeleBronnen)
    batchInsert(deelBronnen)

    // nee, we gaan niet aggregeren .... schiet je niets mee op.
    // allFields.filter(f => f.contains("Level1") && f.toLowerCase.matches(".*(year).*")).foreach(aggregateDateToLevel2(_))
  }

  case class PostProccesing(collection: String, sheet: Option[String])
  {
    def sheetWhere = sheet.map(s => s" and filenaam ~ '$s'").getOrElse("")
    def setGlobal(fieldName: String, value: String):Unit = ditBase.runStatement(s"update cleaned_metadata set $fieldName='$value' where collectie ~ '$collection' $sheetWhere")
    def transfer(fieldNameFrom: String, fieldNameTo: String): Unit = ditBase.runStatement(s"update cleaned_metadata set $fieldNameTo=$fieldNameFrom where " +
      s" ($fieldNameTo is  null or $fieldNameTo = '') " +
      s" and collectie ~ '$collection' $sheetWhere")

    def setGlobal(l: List[(String, String)]):Unit = l.foreach(x => setGlobal(x._1, x._2))
    def transfer(l: List[(String, String)]):Unit = l.foreach(x => transfer(x._1, x._2))

    def setForOneRecord(recordId: String, fieldName: String, value: String) = ditBase.runStatement(s"update cleaned_metadata set $fieldName='$value' where collectie ~ '$collection' and record_id='$recordId'")

    def perRecord(l: List[(String, String, String)]):Unit = l.foreach(x => setForOneRecord(x._1, x._2, x._3))

    def someDefaults = setGlobal(List(
      "factualityLevel1" -> "non-fictie",
      "factualityLevel2" -> "non-fictie",
      "statusTextLevel1" ->"geen vertaling",
      "statusTextLevel2" -> "geen vertaling",
      "corpusProvenance" -> "CLVN",
      "processingMethod" -> "born digital",  //  ???
      "ipr" -> "extern"
    ))

    def defaultTransfer = transfer(List(
      "witnessDayLevel1_from" -> "witnessDayLevel1_to",
      "witnessMonthLevel1_from" -> "witnessMonthLevel1_to",
      "witnessYearLevel1_from" -> "witnessYearLevel1_to",

      "witnessDayLevel1_from" -> "textDayLevel1_from",
      "witnessMonthLevel1_from" -> "textMonthLevel1_from",
      "witnessYearLevel1_from" -> "witnessYearLevel1_from",

      "textDayLevel1_from" -> "textDayLevel1_to",
      "textMonthLevel1_from" -> "textMonthLevel1_to",
      "textYearLevel1_from" -> "textYearLevel1_to",

      "localization_provinceLevel1" -> "localization_provinceLevel2"
    ))

    def apply():Unit = ???
  }


  def setKloekeCodes = ditBase.runStatement(
    """
      |update cleaned_metadata set localization_kloekecodeLevel1=witness_localizations.kloeke_code from witness_localizations where
      |cleaned_metadata.localization_placelevel1=witness_localizations.witnesslocalization_place and cleaned_metadata.localization_provincelevel1=witness_localizations.witnesslocalization_province
    """.stripMargin)

  val patches = List(delflandPatch, elburgPatch, enschedePatch, seyger1Patch, seyger2Patch)

  def main(args:Array[String]):Unit =
  {
    //createUnifiedMetadataTable()
    //ditBase.replaceAll("cleaned_metadata", "?", "")
    setKloekeCodes
    //patches.foreach(_.apply())
  }
}
