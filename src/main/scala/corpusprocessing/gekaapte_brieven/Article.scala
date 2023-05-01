package corpusprocessing.gekaapte_brieven
import java.time.LocalDateTime.now
import scala.xml._
import java.io.File
object Article {

  val nederlabConversie = "/mnt/Projecten/Corpora/Historische_Corpora/Nederlab/gekaaptebrieven/"
   lazy val nederBrieven = new File(nederlabConversie).listFiles().filter(_.getName.endsWith(".xml")).iterator.map(XML.loadFile)


  lazy val nederDivs = nederBrieven.flatMap(x => x \\ "div1").map(d => {
     val comments = d.descendant.filter(_.isInstanceOf[Comment])
     val strippedComments = comments.map(x => x.toString.trim.replaceAll("<!--","").replaceAll("-->","").replaceAll(",.*$","").trim) //  Brief id: 1409, Sourcetabel: table-1409.xml
     // Console.err.println(strippedComments)
     val possibleId = strippedComments.filter(x => x.matches("Brief id:\\s*[0-9]+")).map(x => x.replaceAll("Brief id:\\s*" , "")).headOption
    if (possibleId.isEmpty || !possibleId.map(_.matches("[0-9]+")).get) {
      Console.err.println("Id not found:" + (possibleId,d.toString()))
    }
    (possibleId,d.toString())
  }).filter(_._1.nonEmpty).map({case (id,div) => (id.get.toInt, div)}) // .take(100)

  /*
  <!--Brief id: 23-->
   */

  def main(args: Array[String]) = {
    nederDivs.foreach(println)
  }
}

case class Article(m: Map[String,String]) {

  lazy val xml = <TEI>
    <teiHeader>
      <fileDesc>
        <titleStmt>
          <title>{m("afz_naam_lett")} - {m("ontv_naam_lett")}, {m("datering")} </title>
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
        <listBibl type="allMetadata">
          {m.filter({case (n,v) => v != null && v.length < 100 && v.nonEmpty && !Set("t","f").contains((v))}).toList.sorted.map({case (n,v) => s"$n: $v"}).mkString("\n")}
        </listBibl>
        <listBibl type="inlMetadata">
          <bibl>

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
       <body><div>
         {XML.loadString(m("xml"))}
       </div></body>
    </text>
  </TEI>
}

/*
adressering, adressering_anders, adressering_anders_eq, adressering_anders_xl, adressering_eq, adressering_xl,
afz_beroep_gecorrigeerd_xln, afz_beroep_xln, afz_bijzonderheden, afz_bijzonderheden_eq, afz_bijzonderheden_xl, afz_geslacht, afz_geslacht_eq, afz_geslacht_xl, afz_id,
afz_land_lett, afz_land_lett_eq, afz_land_lett_xl, afz_land_norm, afz_land_norm_eq, afz_land_norm_xl,
afz_naam_lett, afz_naam_lett_INT, afz_naam_lett_eq, afz_naam_lett_vs_norm_eq, afz_naam_lett_xl, afz_naam_norm_INT, afz_naam_norm_xln, afz_niet_leesbaar,
 afz_plaats_lett, afz_plaats_lett_eq, afz_plaats_lett_xl, afz_plaats_norm, afz_plaats_norm_eq, afz_plaats_norm_xl,
 afz_regio_lett, afz_regio_lett_eq, afz_regio_lett_xl, afz_regio_norm, afz_regio_norm_eq, afz_regio_norm_xl,
 afz_rol_xln,
 afz_schip_lett, afz_schip_lett_eq, afz_schip_lett_xl, afz_schip_norm_xln,
 afz_straat_lett, afz_straat_lett_eq, afz_straat_lett_xl, afz_taal_eq, afz_taal_xln,
 afz_tekstsoort_eq, afz_tekstsoort_xln,
 afz_toegevoegd_op, archiefnummer_eq, archiefnummer_xln, brief_id, bronvermelding_xln,
 datering, datering_eq, datering_exact, datering_exact_eq, datering_exact_xl, datering_text, datering_text_eq, datering_xl,
 filename, groepID, groepID_INT, groepID_eq, groepID_xl,
 nederlab_eq, nederlab_groepID, nederlab_groepID_eq, nederlab_id, onduidelijk,
 ontv_beroep_xln, ontv_bijzonderheden, ontv_bijzonderheden_eq, ontv_bijzonderheden_xl, ontv_geslacht, ontv_geslacht_eq, ontv_geslacht_xl, ontv_id,
 ontv_land_lett, ontv_land_lett_eq, ontv_land_lett_xl, ontv_land_norm, ontv_land_norm_eq,
 ontv_land_norm_xl, ontv_naam_lett, ontv_naam_lett_INT, ontv_naam_lett_eq, ontv_naam_lett_vs_norm_eq, ontv_naam_lett_xl, ontv_naam_norm_INT, ontv_naam_norm_xln, ontv_niet_leesbaar,
 ontv_plaats_lett, ontv_plaats_lett_eq, ontv_plaats_lett_xl, ontv_plaats_norm, ontv_plaats_norm_eq, ontv_plaats_norm_xl,
 ontv_regio_lett, ontv_regio_lett_eq, ontv_regio_lett_xl, ontv_regio_norm, ontv_regio_norm_eq, ontv_regio_norm_xl, ontv_rol_xln,
 ontv_schip_lett, ontv_schip_lett_eq, ontv_schip_lett_xl, ontv_schip_norm_xln, ontv_straat_lett, ontv_straat_lett_eq, ontv_straat_lett_xl,
 ontv_taal_eq, ontv_taal_xln, ontv_tekstsoort_eq, ontv_tekstsoort_xln, ontv_toegevoegd_op,
 opmerking, originele_vindplaats_xln, predic_nederlab_id, problemen, row_nr,
 samenvatting, samenvatting_eq, samenvatting_xl,
 taal_INT, taal_xln, tekstsoort, tekstsoort_INT, tekstsoort_anders, tekstsoort_eq, tekstsoort_gecorr_eq, tekstsoort_gecorrigeerd_xln, tekstsoort_xl, toegevoegd_op, transcriptie, transcriptie_onduidelijk
 */