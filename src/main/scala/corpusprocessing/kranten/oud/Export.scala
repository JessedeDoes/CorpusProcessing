package corpusprocessing.kranten.oud
import database.DatabaseUtilities._

import java.io.PrintWriter
import scala.xml._
import Settings._

import scala.util.{Success, Try}

case class ExportTo(exportDir: String) {


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

  val exportQuery_old =   """\"Krantenmetadata17eeeuwdefintieveversie1-22021nw\" a,
                        | (select kb_page, kb_issue, subissue from pages_kb) p,
                        | (select kb_issue, subissue, datum_issue, colophon_int,  to_char(datum_issue, 'Day') as weekday from issues_kb ) i
                        | where
                        |   a.kb_page=p.kb_page and p.kb_issue=i.kb_issue and p.subissue=i.subissue"""

  val exportQuery1664 = "articles_int where cast(issue_date as text) ~ '1664'"
  val exportQuery = "articles_int"
  val exportQuery_geenDubbel = " (select articles_int.* from articles_int, issues_kb_fixed where articles_int.kb_issue=issues_kb_fixed.kb_issue and not dubbel_mag_weg) x"
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

  val q = Select(r =>  {

    def x(s: String) = r.getStringNonNull(s).trim;
    val date = x("issue_date")
    val year = date.replaceAll("-.*", "")
    val decade = if (year.matches("[0-9]{4}")) year.replaceAll(".$", "0") else "undefined"
    val month = date.replaceAll("^.*?-","").replaceAll("-.*","")
    val day =  date.replaceAll(".*-","")
    val delpher_link = s"https://www.delpher.nl/nl/kranten/view?coll=ddd&identifier=${x("kb_article_id")}"
    val pid = s"kranten_17_${x("record_id")}"
    val tekststoort = x("tekstsoort_int").replaceAll("colo.*n|mededeling.advertentie|--", "")
    def ig(n: String, v: String) = <interpGrp type={n}><interp>{cleanAmp(v)}</interp></interpGrp>
    def i(n: String) = <interpGrp type={nameMap.getOrElse(n,n)}><interp>{cleanAmp(x(n))}</interp></interpGrp>

    val article = {
      val a = x("article_text")
      val parsed = Try(XML.loadString("<art>" + vreselijkeTabjes.processTabjes(a) + "</art>").child) match {
        case Success(value) =>  {
          // Console.err.println("Yes, parsed! ")
          value }
        case _ => {
          Console.err.println(s"\nKan niet parsen: ${a.substring(0,Math.min(100,a.length))}")
            a.replaceAll("<i>|</i>|<b>|</b>|\\{tab\\}","")
        }
      }
      parsed
    }



    val now = java.time.LocalDate.now

    val tei  = <TEI>
      <teiHeader>
        <fileDesc>
          <titleStmt>
            <title>{x("paper_title")}, {x("issue_date")}; {x("header")}, {x("subheader")}</title>
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

            {ig("articleClass", tekststoort)}
            {i("paper_title")}
            {i("header_int")}
            {i("subheader_int")}
            {i("land_int")}
            {i("plaats_int")}
            {i("colophon")}
          </bibl>
        </listBibl>export_1664.xml
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
         <head>{x("subheader")}</head>
         <p>
           {article}
         </p>
       </div>
       </body>
     </text>
    </TEI>
    (year,tei)
  },
  exportQuery_geenDubbel.stripMargin)

  def export(): Unit = {
   // year_map.values.foreach(x => x.println("<teiCorpus>"))
    // krantendb.runStatement("update articles_int set land_roland=distinct_headers.land from distinct_headers where articles_int.header=distinct_headers.header;")
    krantendb.iterator(q).foreach(
      { case (y, n) =>
        if (year_map.contains(y)) {
          year_map(y).println(n.toString())
        }
      })

    year_map.values.foreach(x => {
      if (!x.empty) {
        x.println("</teiCorpus>")
        x.pw.close()
      }
    })
  }
}

object Export {
  val exportDir = "/tmp/Krantjes/" // "/mnt/Projecten/Corpora/Historische_Corpora/17e-eeuwseKranten/ExportTestGeenDubbel/"
  def main(args: Array[String]): Unit = {
    new ExportTo(args.headOption.getOrElse(exportDir)).export()
  }
}


/*
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