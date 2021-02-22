package corpusprocessing.edges
import scala.xml._
import java.io.File
import java.util.UUID

import utils.PostProcessXML

object edges {
  val metadata = Map(
    "Delftse_bijbel" -> Map(
      "date" -> "1477",
      "title" -> "Delftse bijbel",
      "decade" -> "1470",
      "genre" -> "bible",
      "factualityLevel1" -> "religious"
    ),
    "DeuxAes" -> Map(
      "date" -> "1562",
      "title" -> "Deux Aes",
      "decade" -> "1560",
      "genre" -> "bible",
      "factualityLevel1" -> "religious"
    ),
    "Lutherse_bijbel" -> Map(
      "date" -> "1648",
      "title" -> "Lutherse bijbel",
      "decade" -> "1640",
      "genre" -> "bible",
      "factualityLevel1" -> "religious"
    ),
    "SV1657_xml_dbnl" -> Map(
      "date" -> "1657",
      "title" -> "Statenvertaling 1657",
      "decade" -> "1650",
      "genre" -> "bible",
      "factualityLevel1" -> "religious"
    ),
    "Leuvense_bijbel" -> Map(
      "date" -> "1548",
      "title" -> "Leuvense bijbel",
      "decade" -> "1540",
      "genre" -> "bible",
      "factualityLevel1" -> "religious"
    )
  )

  val leenwoordenData = "/home/jesse/workspace/leenwoordenzoeker/data/Leenwoordenzoeker_(Leenwrdb-CW-EWB)_laatste_versie.tsv"

  lazy val lwMap = {
    val  lines = io.Source.fromFile(leenwoordenData).getLines().map(_.split("\\t").toList).toStream
    val fields = lines.head
    val values: Map[String, List[(String, Map[String, String])]] = lines.tail.map(l => fields.zip(l).toMap).map(m => m("trefwoord") -> m).groupBy(_._1).mapValues(_.toList)
    val v1 = values.mapValues(_.map(_._2))
    v1
  }

  def interp(n: String,v: String)  = <interpGrp type={n}><interp>{v}</interp></interpGrp>

  def meta(f: String) = {
    val pid:String = UUID.randomUUID().toString
    val m = metadata(f)  ++ Map("pid" ->  pid)


    def interp(n: String,v: String)  = { <interpGrp type={n}><interp>{m(v)}</interp></interpGrp> }
    def interp_1(n: String)  = { <interpGrp type={n}><interp>{m(n)}</interp></interpGrp> }

    <listBibl xml:id="inlMetadata">
    <bibl>
      {interp("titleLevel2", "title")}
      {interp("title", "title")}
      {interp("datering", "date")}
      {interp("decade", "decade")}
      {interp("witnessYearLevel2_from", "date")}
      {interp("witnessYearLevel2_to", "date")}
      {interp("textYearLevel2_from", "date")}
      {interp("textYearLevel2_to", "date")}
      {interp("witnessYear_from", "date")}
      {interp("witnessYear_to", "date")}
      {interp("textYear_from", "date")}
      {interp("textYear_to", "date")}
      {interp_1("factualityLevel1")}
      {interp_1("genre")}
      {interp_1("pid")}
    </bibl>
    </listBibl>
  }

  val edges_in="/mnt/Projecten/Corpora/Historische_Corpora/EDGeS_historical_bible_corpus/Tagged/";
  val edges_out="/mnt/Projecten/Corpora/Historische_Corpora/EDGeS_historical_bible_corpus/WithMetadata/"

  val nice=true;

  def  addProcessedMetadataValues(d: Elem, m: Elem) = {
    PostProcessXML.updateElement(d, _.label == "sourceDesc", e => e.copy(child = e.child ++ Seq(m)))
  }

  def addSourceLanguage(w: Elem)  =
    {
      val lemma = (w \ "@lemma").text.toLowerCase
      val sourceLanguages = lwMap.getOrElse(lemma, List()).map(_("geleend_uit")).mkString("|")
      if (sourceLanguages.nonEmpty) w.copy(attributes = w.attributes.append(new UnprefixedAttribute("sourceLanguage", sourceLanguages, Null))) else w
    }

  def fixFile(in: String, out: String) =
  {
    val dir = new File(in).getParentFile.getName
    println(dir)
    val m = meta(dir)
    val d = addProcessedMetadataValues(XML.load(in), m)
    val d1 = PostProcessXML.updateElement(d, _.label == "w", addSourceLanguage)
    if (this.nice) {
      val p = new scala.xml.PrettyPrinter(300, 4)
      val t = p.format(d1)
      val w = new java.io.PrintWriter(out)
      w.write(t)
      w.close()
    } else XML.save(out, d1,  enc="UTF-8")
  }

  def main(args: Array[String]): Unit = {
    utils.ProcessFolder.processFolder(new File(edges_in), new File(edges_out), fixFile, parallel = false)
  }
}


/*
Delftse_bijbel
DeuxAes
Leuvense_bijbel
Lutherse_bijbel
SV1657_xml_dbnl

 */