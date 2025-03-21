package corpusprocessing.kranten.oud
import database.DatabaseUtilities._

import java.io.PrintWriter
import scala.xml._
import Settings._

import scala.util.{Success, Try}

import TEIScope._

case class ExportTo(exportDir: String) {

   val hyphenLog = new PrintWriter("/tmp/hyphens.log")
  val parseLog = new PrintWriter("/tmp/parse.log")
  /*
      Column     | Type | Collation | Nullable | Default
---------------+------+-----------+----------+---------
 record_id     | text |           | not null |
 kb_article_id | text |           |          |
 kb_issue      | text |           |          |
 subissue      | text |           |          |
 kb_page       | text |           |          |
 colophon      | text |           |          |
 issue_date    | date |           |          |
 paper_title   | text |           |          |
 land          | text |           |          |
 plaats        | text |           |          |
 tekstsoort    | text |           |          |
 header        | text |           |          |
 subheader     | text |           |          |
 article_text  | text |           |          |
Indexes:

   */
  val nameMap = Map(
    "issue_date" -> "witnessDate_from",
    "tekstsoort_int" -> "articleClass",
    "paper_title" -> "titleLevel2", // of moet dat 3 wezen??
    "subheader_int" -> "titleLevel1",
    "header_int" -> "newspaperSection",
    "record_id" -> "sourceID",
    "plaats_int" -> "settingLocation_place",
    "land_int" -> "settingLocation_country",
    "colophon" -> "colophon",
    //"weekday" -> "witnessDoWLevel1_from"
  )



  case class SillyThing(fileName: String) {
    var empty = true;
    lazy val pw: PrintWriter = {
      val p = new PrintWriter(fileName)
      p.print("<teiCorpus>")
      p
    }
    def println(s: String)  = { empty = false; pw.write(s); pw.flush() }
  }

  val year_map = (1600 to 1700).map(i => i.toString -> new SillyThing(s"$exportDir/export_$i.xml")).toMap

  val restje = "export_"

  def cleanAmp(s:String) = s.replaceAll("&amp([^;])","&$1").replaceAll("&amp;", "&")

  def escapeEq(x:String)  = x.replaceAll("=", "空白")
  import scala.util.matching.Regex
  val pattern: Regex = "<([^<>]*)>".r

  def fixHyphens(a: String) = {
    val dehyph = a
      .replaceAll("</i>([= ]*)<i>","$1")
      .replaceAll("<i></i>", "")
      .replaceAll("<b></b>", "")
      .replaceAll("<b>([= ]+)</b>","$1")
      .replaceAll("<i>([= ]+)</i>","$1")
      .replaceAll("<[bi]/>","")
      .replaceAll("([^\\s<>=]+)(\\s*=+\\s*)([^\\s<>=]+)", "$1$3 <anchor type='a2b' org=\"$1|$3\"/>")

      .replaceAll("([^\\s<>=]+) *([=]+) *[\r\n]+ *([^\\s<>=]+)", "$1$3 <anchor type='a2a' org=\"$1|$3\"/>\n")

      .replaceAll("([^\\s<>]+) *([-]) *[\r\n]+ *([a-z][^\\s<>]+)", "$1$3 <anchor type='a3' org=\"$1|$3\"/>\n")

    val d2 = pattern.replaceAllIn(dehyph, m => s"<${escapeEq(m.group(1))}>").replaceAll("\\s*=+\\s*", "")

    if (d2.contains("=")) {
      "(.{0,20})(=+)(.{0,20})".r.findAllMatchIn(d2).foreach(m => {
        hyphenLog.println("MISSED:" + m.toString().replaceAll("\\s+", " "))
      })
    }

    val d3 = d2.replaceAll("空白","=")

    import util.matching.Regex._
    import util.matching._
    "(.{0,20})<anchor[^<>]*>(.{0,20})".r.findAllMatchIn(d3).foreach(m => {
      hyphenLog.println(m.toString().replaceAll("\\s+", " "))
    })
    "(.{0,20})(=+[^'\"])(.{0,20})".r.findAllMatchIn(d3).foreach(m => {
      hyphenLog.println("MISSED:" + m.toString().replaceAll("\\s+", " "))
    })
    d3
  }

  val q = Select(r =>  {

    def x(s: String) = r.getStringNonNull(s).trim;

    def getCleanedText(s: String)  = {
      val a0 = cleanAmp(r.getStringNonNull(s).trim);
      val a = fixHyphens(a0)
      val parsed = Try(XML.loadString("<art>" + vreselijkeTabjes.processTabjes(a) + "</art>").child) match {
        case Success(value) =>  {
          // Console.err.println("Yes, parsed! ")
          value }
        case _ => {
          Console.err.println(s"\nKan niet parsen: ${a.substring(0,Math.min(100,a.length))}")
          parseLog.println(s"\n### PARSE ERROR\nKan niet parsen: ${a}")
          a.replaceAll("<i>|</i>|<b>|<anchor[^<>]*>|</b>|\\{tab\\}","")
        }
      }
      parsed
    }

    val date = x("issue_date")
    val year = date.replaceAll("-.*", "")
    val decade = if (year.matches("[0-9]{4}")) year.replaceAll(".$", "0") else "undefined"
    val month = date.replaceAll("^.*?-","").replaceAll("-.*","")
    val day =  date.replaceAll(".*-","")
    val delpher_link =  {
      val article_id = x("kb_article_id")
      val issue_id =  x("kb_issue")
      if (article_id.nonEmpty)
      s"https://www.delpher.nl/nl/kranten/view?coll=ddd&identifier=${article_id}"
      else   { // colofons hebben geen artikel id en ook geen page id
        val z = s"https://www.delpher.nl/nl/kranten/view?coll=ddd&identifier=${issue_id}:mpeg21"
        println(z)
        z
      }
    }
    val pid = s"kranten_17_${x("record_id")}"
    val tekstsoort = x("tekstsoort_int").replaceAll("xcolo.*n|mededeling.advertentie|--", "")
    def ig(n: String, v: String) = <interpGrp type={n}><interp>{cleanAmp(v)}</interp></interpGrp>
    def i(n: String) = <interpGrp type={nameMap.getOrElse(n,n)}><interp>{getCleanedText(n)}</interp></interpGrp>

    val article = {
      //perl -pe 's/=((<\/?[ib]\/?>)?)\s*$/\n/'
      // perl -pe 's/=((</?[ib]/?>)?)\\s*$/$1/'

      val a = x("article_text") // .replaceAll("=((</?[ib]/?>)?)\\s+", "$1===") // Huh????? Waarom????

      val a1 = fixHyphens(a)

      val parsed = Try(XML.loadString("<art>" + vreselijkeTabjes.processTabjes(a1) + "</art>").child) match {
        case Success(value) =>  {
          // Console.err.println("Yes, parsed! ")
          value }
        case _ => {
          // Console.err.println(s"\nKan niet parsen: ${a.substring(0,Math.min(100,a.length))}")
            parseLog.println(s"\n### PARSE ERROR\nKan niet parsen: ${a1}")
            a1.replaceAll("<i>|</i>|<b>|</b>|<anchor[^<>]*>|\\{tab\\}","")
        }
      }
      parsed
    }



    val now = java.time.LocalDate.now

    val tei  = <TEI>
      <teiHeader>
        <fileDesc>
          <titleStmt>
            <title>{getCleanedText("paper_title")}, {x("issue_date")}; {getCleanedText("header")}, {getCleanedText("subheader")}</title>
            <respStmt>
              <resp>compiled by</resp>
              <name>Nicoline van der Sijs and volunteers</name>
            </respStmt>
          </titleStmt>
          <publicationStmt>
            <availability><licence>This file may not be redistributed!! It is a preliminary version</licence></availability>
          </publicationStmt>
        </fileDesc>
        <sourceDesc>
        <listBibl type="inlMetadata">
          <bibl>
            {ig("pid",pid)}
            {i("record_id")}

            {ig("witnessYearLevel1_from", year)}
            {ig("decade", decade)}

            {ig("witnessDateLevel1_from", date)}
            {ig("witnessDateLevel2_from", date)}
            {ig("witnessMonthLevel1_from", month)}
            {ig("witnessDayLevel1_from", day)}
            {ig("witnessYearLevel1_to", year)}
            {ig("witnessMonthLevel1_tp", month)}
            {ig("witnessDayLevel1_to", day)}

            {ig("witnessYearLevel2_from", year)}
            {ig("witnessMonthLevel2_from", month)}
            {ig("witnessDayLevel2_from", day)}
            {ig("witnessYearLevel2_to", year)}
            {ig("witnessMonthLevel2_to", month)}
            {ig("witnessDayLevel2_to", day)}


            {ig("sourceUrl", delpher_link)}
            {ig("corpusProvenance", "Courantencorpus")}
            {ig("editorLevel3", "Nicoline van der Sijs")}

            {ig("articleClass", tekstsoort)}
            {i("paper_title")}
            {i("header_int")}
            {i("subheader_int")}
            {i("land_int")}
            {i("plaats_int")}
            {i("colophon")}
          </bibl>
        </listBibl>
        </sourceDesc>
        <revisionDesc>
          <list>
            <item>Preliminary version, exported <date>{now}</date> with duplicates, issue issues, segmentation errors and metadata inaccuracies!!!!!!</item>
          </list>
        </revisionDesc>
      </teiHeader>
     <text>
       <body>
       <div>
         <head>{getCleanedText("subheader_int")}</head>
         <p>
           {article}
         </p>
       </div>
       </body>
     </text>
    </TEI>
    (year,setTeiScope(tei))
  },

    exportQuery_geenDubbelMetWatMeer.stripMargin)

  def export(): Unit = {
   // year_map.values.foreach(x => x.println("<teiCorpus>"))
    // krantendb.runStatement("update articles_int set land_roland=distinct_headers.land from distinct_headers where articles_int.header=distinct_headers.header;")
    krantendb.iterator(q).foreach(
      { case (y, n) =>
        if (year_map.contains(y)) {
          year_map(y).println(n.toString())
          hyphenLog.flush()
          parseLog.flush()
        }
      })

    year_map.values.foreach(x => {
      if (!x.empty) {
        x.println("</teiCorpus>")
        x.pw.close()
      }
    })
    hyphenLog.close()
    parseLog.close()
  }
}

object Export {
  val exportDir = "/mnt/Projecten/Corpora/Historische_Corpora/17e-eeuwseKranten/Export2025_3/"
  def main(args: Array[String]): Unit = {
    new ExportTo(args.headOption.getOrElse(exportDir)).export()
  }
}


/*

 update issues_kb_fixed set dubbel_mag_weg=true from issues_kb_fixed i
 where issues_kb_fixed.issue_handled and i.issue_handled
 and i.datum_issue=issues_kb_fixed.datum_issue and i.paper_title_int = issues_kb_fixed.paper_title_int
 and (i.text_length > issues_kb_fixed.text_length or (i.text_length > issues_kb_fixed.text_length and i.tellertje > issues_kb_fixed.tellertje))


create table landcheck as SELECT articles_int.land_header,
    articles_int.plaats_int AS plaats,
    articles_int.land_int,
    (array_agg(DISTINCT articles_int.header_int))[0:5] AS some_headers,
    sum(1) AS aantal
   FROM articles_int
  WHERE articles_int.land_int <> articles_int.land_header
  GROUP BY articles_int.land_int, articles_int.plaats_int, articles_int.land_header
  ORDER BY articles_int.land_header, articles_int.land_int, articles_int.plaats_int;

 */