package corpusprocessing.kranten.oud
import database.DatabaseUtilities._

import java.io.PrintWriter
import scala.xml._

object Export {
  val krantenconfig = new database.Configuration(
    name = "krantjes",
    server = "svowdb20.ivdnt.loc",
    database = "kranten_metadatabewerking_2021",
    user = "postgres",
    password = "inl")

  val krantendb = new database.Database(krantenconfig)

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
    "tekstsoort" -> "articleClass",
    "paper_title" -> "titleLevel2", // of moet dat 3 wezen??
    "subheader" -> "titleLevel1",
    "header" -> "newspaperSection",
    "record_id" -> "sourceID",
    "plaats" -> "settingLocation_place",
    "land" -> "settingLocation_country",
    "colophon" -> "colophon",
    //"weekday" -> "witnessDoWLevel1_from"
  )

  val exportDir = "/mnt/Projecten/Corpora/Historische_Corpora/17e-eeuwseKranten/Export/"

  val exportQuery =   """\"Krantenmetadata17eeeuwdefintieveversie1-22021nw\" a,
                        | (select kb_page, kb_issue, subissue from pages_kb) p,
                        | (select kb_issue, subissue, datum_issue, colophon_int,  to_char(datum_issue, 'Day') as weekday from issues_kb ) i
                        | where
                        |   a.kb_page=p.kb_page and p.kb_issue=i.kb_issue and p.subissue=i.subissue"""

  val exportQuery1 = "articles_int"

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

  val q = Select(r =>  {

    def x(s: String) = r.getStringNonNull(s).trim;
    val date = x("issue_date")
    val year = date.replaceAll("-.*", "")
    val month = date.replaceAll("^.*?-","").replaceAll("-.*","")
    val day =  date.replaceAll(".*-","")
    val delpher_link = s"https://www.delpher.nl/nl/kranten/view?coll=ddd&identifier=${x("kb_article_id")}"
    val pid = s"kranten_17_${x("record_id")}"
    def ig(n: String, v: String) = <interpGrp type={n}><interp>{v}</interp></interpGrp>
    def i(n: String) = <interpGrp type={nameMap.getOrElse(n,n)}><interp>{x(n)}</interp></interpGrp>
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
            {ig("corpusProvenance", "The van der Sijs newspaper Corpus")}
            {ig("editorLevel3", "Nicoline van der Sijs")}

            {i("tekstsoort")}
            {i("paper_title")}
            {i("header")}
            {i("subheader")}
            {i("land")}
            {i("plaats")}
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
       <div>
         <head>{x("subheader")}</head>
         <p>
           {x("article_text")}
         </p>
       </div>
     </text>
    </TEI>
    (year,tei)
  },
  exportQuery1.stripMargin)

  def export(): Unit = {
   // year_map.values.foreach(x => x.println("<teiCorpus>"))

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

  def main(args: Array[String]): Unit = {
    export()
  }
}

/*

https://www.delpher.nl/nl/kranten/view?coll=ddd&identifier=ddd:010411983:mpeg21:p001
                  Table "public.Krantenmetadata17eeeuwdefintieveversie1-22021nw"
                        Column                         |  Type   | Collation | Nullable | Default
-------------------------------------------------------+---------+-----------+----------+----------
 record_id                                             | text    |           | not null |
 article_text_gesplitst                                | text    |           |          |
 alert_land_nw_niet_gelijk_aan_land_nw_ACHTERHAALD     | text    |           |          |
 alert_plaats_nw_niet_gelijk_aan_plaats_nw_ACHTERHAALD | text    |           |          |
 article_id                                            | text    |           |          |
 article_scannr_KB                                     | text    |           |          |
 article_scannr_KB_pluspagina                          | text    |           |          |
 article_text_CS                                       | text    |           |          |
 article_title_KB                                      | text    |           |          |
 article_title_nw                                      | text    |           |          |
 colophon                                              | text    |           |          |
 colophon_text_CS                                      | text    |           |          |
 colophon_text_nw                                      | text    |           |          |
 commentaar_bewerker                                   | text    |           |          |
 comments_corrector                                    | text    |           |          |
 date_KB                                               | text    |           |          |
 err_text_type                                         | text    |           |          |
 header_autom                                          | text    |           |          |
 header_nw                                             | text    |           |          |
 id                                                    | text    |           |          |
 id_uniek                                              | text    |           |          |
 land_nw                                               | text    |           |          |
 land_nw_ACHTERHAALD                                   | text    |           |          |
 paper_id                                              | text    |           |          |
 paper_title_KB                                        | text    |           |          |
 paper_title_nw                                        | text    |           |          |
 plaats_nw                                             | text    |           |          |
 plaats_nw_ACHTERHAALD                                 | text    |           |          |
 subheader_autom                                       | text    |           |          |
 subheader_nw                                          | text    |           |          |
 tekstsoort_nw                                         | text    |           |          |
 kan_weg                                               | boolean |           |          | false
 opmerking                                             | text    |           |          | ''::text
 has_comment                                           | boolean |           |          | false
 commentaar_kopie                                      | text    |           |          | ''::text
 commentaar_legen                                      | text    |           |          | ''::text
 paper_title_int                                       | text    |           |          |
 article_text_int                                      | text    |           |          |
 article_title_int                                     | text    |           |          |
 header_int                                            | text    |           |          |
 subheader_int                                         | text    |           |          |
 datum_int                                             | date    |           |          |
 tekstsoort_int                                        | text    |           |          |
 kb_page                                               | text    |           |          |
 subissue                                              | text    |           |          |
 kb_issue                                              | text    |           |          |
 no_issue_issues                                       | boolean |           |          | false
Indexes:
    "export_pkey" PRIMARY KEY, btree (record_id)

 */
