package corpusprocessing.nieuwe_tijdingen

import database.DatabaseUtilities.Select
import utils.PostProcessXML._
import utils.ProcessFolder

import java.io.{File, FileWriter, PrintWriter}
import scala.xml._

object nieuwetijdingen2tei {
  val input = "/mnt/Projecten/Corpora/Historische_Corpora/NieuweTijdinghen/Teksten/AllesWelgevormd/"
  val output = "/data/NieuweTijdingen/TEI/" // "/mnt/Projecten/Corpora/Historische_Corpora/NieuweTijdinghen/Teksten/OpWegNaarTEI/"
  lazy val outList = new FileWriter("/tmp/tijdingen.tsv")
  val scanDir = "/mnt/Projecten/Corpora/Historische_Corpora/NieuweTijdinghen/Scans/PDF/"

  val krantenconfig = new database.Configuration(
    name = "krantjes",
    server = "svowdb20.ivdnt.loc",
    database = "nieuwe_tijdingen",
    user = "postgres",
    password = "inl")

  val db = new database.Database(krantenconfig)


  val fields = "titleFromFilename,titleFromDoc,datumFromDoc,jaar,maand,dag,filePath,scan,distance".split(",")
  outList.write(fields.mkString("\t") + "\n")

  val maandMapping = "jan|feb|mar|apr|mei|jun|jul|aug|sep|okt|nov|dec".split("\\|").zipWithIndex.map{case (m,i) => m -> (i+1)}.toMap

  val mdPattern = "([0-9]{1,2}[_ ])?+(jan|feb|mar|apr|mei|jun|jul|aug|sep|okt|nov|dec)".r


  lazy val allScans = new File(scanDir).listFiles.map(_.getName)

  def loadInDB() = {
    db.runStatement("drop table if exists plaatsmatch_upload_new")
    db.runStatement(
      s"""create table plaatsmatch_upload_new (
            ${fields.map(f => f.toLowerCase + " text").mkString(", ")}
         )
        """)
    db.loadFile("plaatsmatch_upload_new", new java.io.File("/tmp/tijdingen.tsv"))
  }

  def metadataFromFilename(filePath: String, doc: Elem): (Elem, String) = {

    val baseFileName = filePath.replaceAll(".xml", "").replaceAll(".*/", "").replaceAll(", ontkleurd", "")
    val year = baseFileName.replaceAll("_.*", "")
    val zonderJaar = baseFileName.replaceAll("^[0-9]{4}_", "")
    val md = mdPattern.findFirstIn(zonderJaar)
    val (m,d) = md.map(s => {
      val m = s.replaceAll("[^a-z]", "")
      val d = s.replaceAll("[^0-9]", "")
      (maandMapping.get(m), if (d.nonEmpty) Some(d.toInt) else None)
    }).getOrElse((None,None))

    val maybeScan = new File(scanDir +  baseFileName + ".pdf")

    val (scan,confidence) = if (maybeScan.exists()) (maybeScan.getCanonicalPath, 0) else {
      val gok = allScans.minBy(f => utils.EditDistance.ciDistance(f.replaceAll(".pdf", ""),baseFileName))
      val dist = utils.EditDistance.ciDistance(gok.replaceAll(".pdf", ""),baseFileName)
      gok -> dist
    }
    val titleFromFilename = mdPattern.replaceAllIn(zonderJaar, "").replaceAll("^[0-9]+_", "").replaceAll("^_", "")
    val titleFromDoc = (doc \\ "titel").text.replaceAll("\\s+", " ").trim
    val datumFromDoc= (doc \\ "datum").text.replaceAll("\\s+", " ").trim


    val metaList = List(titleFromFilename,titleFromDoc,datumFromDoc,year,m.getOrElse(""),d.getOrElse(""), filePath, scan, confidence)

    outList.write(metaList.mkString("\t") + "\n")

    val meta =  <listBibl type="inlMetadata" xml:id="inlMetadata">
      <bibl>
        <interpGrp type="titleLevel1"><interp>ZN kranten 17: {titleFromFilename}</interp></interpGrp>
        <interpGrp type="titleLevel2"><interp>Nieuwe tijdingen</interp></interpGrp>
        <interpGrp type="corpusProvenance"><interp>Corpus Zuid-Nederlandse Historische Kranten</interp></interpGrp>
        <interpGrp type="witnessYearLevel1_from"><interp>{year}</interp></interpGrp>
        {m.map(x => <interpGrp type="witnessMonthLevel1_from"><interp>{x}</interp></interpGrp>).getOrElse(Seq())}
        {d.map(x => <interpGrp type="witnessDayLevel1_from"><interp>{x}</interp></interpGrp>).getOrElse(Seq())}

        <interpGrp type="witnessYearLevel1_to"><interp>{year}</interp></interpGrp>
        {m.map(x => <interpGrp type="witnessMonthLevel1_to"><interp>{x}</interp></interpGrp>).getOrElse(Seq())}
        {d.map(x => <interpGrp type="witnessDayLevel1_to"><interp>{x}</interp></interpGrp>).getOrElse(Seq())}
      </bibl>
      </listBibl>
    (meta, titleFromFilename)
     // println(s"$year // $md1 // $title")
  }

  def fixDocje(e: Elem, meta: Elem, title: String):Elem  = {
    updateElement(e, _.label=="teiHeader", h =>
      <teiHeader>
      <fileDesc>
      <titleStmt>
        <title>{title}</title>
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
        {meta}
        </sourceDesc>
      </teiHeader>
    )
  }


  def main(args: Array[String]): Unit = {

    ProcessFolder.processFolder(new File(input), new File(output), {case (i,o) =>
      if (i.endsWith(".xml")) {
        val g = new File(i)
        val inDoc = XML.loadFile(g)
        val (meta, title) = metadataFromFilename(i, inDoc)
        val outDoc = fixDocje(inDoc, meta, title)
        //val tsvLine = Metadata.convertTeiMetadataToTsv(outDoc)
        //val outTSV = o.replaceAll(".xml$", ".tsv")
       // println(tsvLine)
        XML.save(o, outDoc, "UTF-8")
      }
    })

    outList.close()
    loadInDB()
  }
}


object loadTextIntoDB
{
  import nieuwetijdingen2tei.db

  lazy val bestanden = db.slurp(Select(r => r.getString("filepath"), "bestanden"))

  def main(args: Array[String]): Unit = {
    bestanden.foreach(b => {
      val content = io.Source.fromFile(b).getLines().mkString("\n").replaceAll("'", "''") // .replaceAll("\\s+", " ")
      val b1 = b.replaceAll("'", "''")
      val q = s"update content set content='$content' where filepath='${b1}'"
      db.runStatement(q)
    })
  }
}
/*
1621_84_Puncten ende articulen wat die van de stadt Ellenboghen, ontkleurd.xml
1621_85_Verhael van de victorie die den grave van Bucquoy vercreghen heeft, ontkleurd.xml
1621_86 Den Eedt met den welcken Bethlem Gabor, ontkleurd.xml
1621_87_11 jun_Nieuwe tijdinge uit Roomen, ontkleurd.xml
1621_88_Verhael van des keysers legher, ontkleurd.xml
1621_89_16 jun_Nieuwe tijdinge uit Vranckrijck van t’ghene passeert, ontkleurd.xml
1621_8_Coninck feest vanden palatin, ontkleurd.xml
1621_90_Nieuwe Tijdinghe wt Surich in Zwitserlant ende Weenen in Oostenrijck, ontkleurd.xml
1621_91_Tijdinghe uit Hollandt ende Pfaltzgravenlandt, ontkleurd.xml
1621_92_Die tweede gazette des maendts iuny, ontkleurd.xml
1621_93_Gheschreuen_brieff_van_Gabriel_Bethlen, ontkleurd.xml
1621_94_Declaratie vanden coninc van Vranckrijck, ontkleurd.xml
1621_95_Sommatie ghedaen vanweghen des conincx aen monsieur de Soubise, ontkleurd.xml
1621_96_Verhael van de groote victorie die den grave van Bucquoy, ontkleurd.xml

      <teiHeader>
        <fileDesc>
          <titleStmt>
            <title>Ordinarisse middel-weeckse courante, 1648-05-26; , Wt Antwerpen den 22 May.</title>
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
            <interpGrp type="pid"><interp>kranten_17_6678</interp></interpGrp>
            <interpGrp type="sourceID"><interp>6678</interp></interpGrp>
            <interpGrp type="witnessYearLevel1_from"><interp>1648</interp></interpGrp>
            <interpGrp type="witnessMonthLevel1_from"><interp>05</interp></interpGrp>
            <interpGrp type="witnessDayLevel1_from"><interp>26</interp></interpGrp>
            <interpGrp type="witnessYearLevel1_to"><interp>1648</interp></interpGrp>
            <interpGrp type="witnessMonthLevel1_tp"><interp>05</interp></interpGrp>
            <interpGrp type="witnessDayLevel1_to"><interp>26</interp></interpGrp>

            <interpGrp type="witnessYearLevel2_from"><interp>1648</interp></interpGrp>
            <interpGrp type="witnessMonthLevel2_from"><interp>05</interp></interpGrp>
            <interpGrp type="witnessDayLevel2_from"><interp>26</interp></interpGrp>
            <interpGrp type="witnessYearLevel2_to"><interp>1648</interp></interpGrp>
            <interpGrp type="witnessMonthLevel2_to"><interp>05</interp></interpGrp>
            <interpGrp type="witnessDayLevel2_to"><interp>26</interp></interpGrp>


            <interpGrp type="sourceUrl"><interp>https://www.delpher.nl/nl/kranten/view?coll=ddd&amp;identifier=ddd:010680628:mpeg21:a0009</interp></interpGrp>
            <interpGrp type="corpusProvenance"><interp>The van der Sijs newspaper Corpus</interp></interpGrp>
            <interpGrp type="editorLevel3"><interp>Nicoline van der Sijs</interp></interpGrp>

            <interpGrp type="articleClass"><interp>binnenlands nieuws</interp></interpGrp>
            <interpGrp type="titleLevel2"><interp>Ordinarisse middel-weeckse courante</interp></interpGrp>
            <interpGrp type="newspaperSection"><interp/></interpGrp>
            <interpGrp type="titleLevel1"><interp>Wt Antwerpen den 22 May.</interp></interpGrp>
            <interpGrp type="settingLocation_country"><interp>België</interp></interpGrp>
            <interpGrp type="settingLocation_place"><interp>Antwerpen</interp></interpGrp>
            <interpGrp type="colophon"><interp>Ghedruckt tot Amsterdam. Voor de Weduwe van Francoys Lies-houdt, Boeck-verkoopster op den Dam, int Groot-Boeck den 26 May Anno 1648.</interp></interpGrp>
          </bibl>
        </listBibl>
        </sourceDesc>
        <revisionDesc>
          <list>
            <item>Preliminary version, exported <date>2021-09-23</date> with duplicates, issue issues, segmentation errors and metadata inaccuracies!!!!!!</item>
          </list>
        </revisionDesc>
      </teiHeader>

 */