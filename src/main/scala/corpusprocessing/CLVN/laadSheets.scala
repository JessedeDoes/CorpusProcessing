package corpusprocessing.CLVN

object laadSheets
{

  val oldDitSheet="DiT-CorpusQH14.csv"

  def cleanSplit(l:String):Seq[String] = l.split("\\t",-1).map(_.replaceAll("\"", "")).map(_.trim).toSeq

  def sanitizeId(id: String) = { val after = id.trim.replaceAll("[^A-Za-z0-9_.-]","_"); if (false && id != after) println(s"Beter: $id -> $after"); after }

  val maanden = List("jan|januari|january", "feb|februari|february", "mrt|mar|maart|march", "apr|april", "mei|may",  "jun|juni|june", "jul|juli|july", "aug|august|augustus",
    "sep|september", "okt|oct|oktober|october", "nov|november", "dec|december")

  // resource linkjes iets als http://www.itineranova.be/in/SAL7352/R%C2%B0144.2/act

  val drie = "([0-9]+)\\s*[/-]\\s*([0-9]+)\\s*[/-]\\s*([0-9]+)".r
  val tweeMetJaar = "([0-9]+)\\s*[/-]\\s*([0-9]{4})".r
  val twee = "([0-9]+)\\s*[/-]\\s*([0-9]+)".r
  val maandGetal = s"""(${maanden.mkString("|")})(\\s|-)*([0-9]+)""".r

  def parseDay(month_day: String) =
  {
    val dl = month_day.toLowerCase

     val month =
     if (maandGetal.findFirstMatchIn(dl).isDefined)
      maanden.zipWithIndex
      .find({case (m,i) => m.split("\\|").exists(x => dl.contains(x)) } )
      .map({case (m,i) => (i+1).toString})
      else if (drie.findFirstMatchIn(dl).isDefined) drie.findFirstMatchIn(dl).map(_.group(2))
      else if (tweeMetJaar.findFirstMatchIn(dl).isDefined) tweeMetJaar.findFirstMatchIn(dl).map(_.group(1))
      else if (twee.findFirstMatchIn(dl).isDefined) twee.findFirstMatchIn(dl).map(_.group(1))
      else None

     val realDay = if (maandGetal.findFirstMatchIn(dl).isDefined) maandGetal.findFirstMatchIn(dl).map(_.group(3))
         else if  (drie.findFirstMatchIn(dl).isDefined) drie.findFirstMatchIn(dl).map(_.group(1))
         else if (tweeMetJaar.findFirstMatchIn(dl).isDefined) None
         else if (twee.findFirstMatchIn(dl).isDefined) twee.findFirstMatchIn(dl).map(_.group(2))
         else None
      (month,realDay)
   }

  val transcribRegex = "(?i)transcribent\\s*:\\s*(.*?)(\\.|nagekeken)".r
  val folioPat = "([rv]?)[0-9]+([rv]|(\\.[0-9]))"
  val folioRegex = s"(?i)folio:?\\s*($folioPat(\\s*[,-]\\s*$folioPat)*)".r
  
  def parse_aanvullende_informatie(info: String) =
  {
    val transcribent = transcribRegex.findFirstMatchIn(info).map(_.group(1))
    val folio = folioRegex.findFirstMatchIn(info).map(_.group(1))

    //if (info.matches("(?i).*(folio|transcribent).*"))
    // Console.err.println(s"transcribent: $transcribent, folio: $folio in $info")

    (transcribent, folio)
  }

  def enhanceAanvullend(sheet: Sheet) = 
  {
      val newLines = sheet.lines.map(m => 
      {
       val (tr,f) =  parse_aanvullende_informatie(m("aanvullende_informatie"))
       m ++ Map("aanv_trans" -> tr.getOrElse(""),  "aanv_folio" -> f.getOrElse(""))
      })
      val newHeader = sheet.header ++ List("aanv_trans", "aanv_folio")

     sheet.copy(header=newHeader,lines=newLines)
  }

  def enhanceDate(sheet: Sheet):Sheet = // dit niet meer doen .....
  {
     if (sheet.hasField("day")) return sheet // dus dit gebeurt niet meet nu...

     val newLines = sheet.lines.map(m => 
      {
       val (month,day) = parseDay(m("dag"))
       m ++ Map("month" -> month.getOrElse(""),  "day" -> day.getOrElse(""))
      })
      val newHeader = sheet.header ++ List("month", "day")

     sheet.copy(header=newHeader,lines=newLines)
  }

  case class Sheet(file: String, header: List[String], lines:Seq[Map[String,String]])
  {
    def print = lines.foreach(m => println(header.map(m).mkString("\t")))

    def printQuery() = Console.err.println(query)
    
    def hasField(f: String) = header.contains(f)

    def query = s"""
drop table if exists meta;
create table meta
(
  ${header.map(f => s"$f text").mkString(",\n") }
);
\\copy  meta from 'aap' with delimiter E'\\t'
update meta set milestone_id=milestones_id where milestone_id='';
alter table meta drop column milestones_id;

alter table meta add column id_text_part text default null;

update meta set id_text_part=id_div where id_div ~* '[a-z0-9]';
update meta set id_text_part=div_id where div_id ~* '[a-z0-9]';
update meta set id_text_part=id_tekstdeel where id_tekstdeel ~* '[a-z0-9]';
update meta set id_text_part=milestone_id where milestone_id ~* '[a-z0-9]';
update meta set id_text_part=stukje_id where stukje_id ~* '[a-z0-9]';
update meta set id_text_part=stukjes_id where stukjes_id ~* '[a-z0-9]';

alter table meta drop column id_div;
alter table meta drop column div_id;
alter table meta drop column id_tekstdeel;
alter table meta drop column milestone_id;
alter table meta drop column stukje_id;
alter table meta drop column stukjes_id;
"""
  }

  def makeFieldName(s:String) = s.replaceAll("[^A-Za-z0-9_]","_")

  def  readSheet(f:String) =
  {
    val lines = scala.io.Source.fromFile(f).getLines.toStream.map(cleanSplit)
    val firstLine = lines.head
    val rest = lines.drop(1).toStream
    val header = firstLine.toList.map(_.toLowerCase).map(makeFieldName)
    val fileName = f.replaceAll(".*/","")
    val collection = fileName.replaceFirst("_",":").replaceAll("_.*","").replace(":","_")

    def toMap(l:Seq[String]) = l.zipWithIndex.map({case (x,i) => header(i) -> x}).toMap ++ 
        Map("fileNaam" ->fileName, "collectie" -> collection)

    val maps = rest.filter(_.size >= header.size).map(toMap)

    Sheet(f,header ++ List("fileNaam", "collectie"), maps)
  }

  def parseSheets(d: String) =
  {
    val allSheets = new java.io.File(d).listFiles
    .map(_.getCanonicalPath)
    .map(readSheet)
    .filter(x => x.header.contains("record_id") || x.header.contains("milestone_id"))
    
    val allFields = allSheets.flatMap(_.header)
    .groupBy(identity)
    .mapValues(_.size)
    .filter(_._1.trim.size > 0)

    // allFields.foreach(println)

    val allNames = allFields.keySet.toList.sortBy(identity)
    val homoSheets = allSheets.map(s => s.copy
         (header=allNames,
          lines=s.lines.map(m => allNames.map(n => n -> m.getOrElse(n,"")).toMap )))
    homoSheets.map(enhanceDate).map(enhanceAanvullend)
  }


  def main(args: Array[String]) =
  {
     //readSheet(args(0)).lines.foreach(println)
     val sheets = parseSheets(args(0))
     sheets.head.printQuery
     sheets.foreach(_.print)
  }
}
