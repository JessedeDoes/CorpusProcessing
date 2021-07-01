package corpusprocessing.brievenalsbuit

import utils.{PostProcessXML, ProcessFolder}

import java.io.File
import scala.xml.{Elem, Text, XML}

object stripAnnotation {
  def stripAnnotation(d: Elem) = {
    val d0  = PostProcessXML.updateElement5(d, _.label=="w",
      w => { if ((w \ "seg").nonEmpty) (w \ "seg").headOption.map(_.child).getOrElse(Seq()) else w.child }
    ).asInstanceOf[Elem]
    val d1 = PostProcessXML.updateElement5(d0, x => x.label == "pc" || x.label == "c", pc => pc.child).asInstanceOf[Elem]
    val d2 = PostProcessXML.updateElement5(d1, x => x.label == "p" , p => {
      val pt = XML.loadString(p.toString()) // ugly....
      val nc = pt.child.map({
        case t: Text => Text(t.text.replaceAll("\\s+", " "))
        case z => z})
      p.copy(child=nc)
    }).asInstanceOf[Elem]
    val d3 = PostProcessXML.updateElement5(d2, _.label == "lb", lb => {
      Seq(lb, Text("\n"))
    }).asInstanceOf[Elem]
    d3
  }
}

case class wolkenCorpus(source: String, target: String, corpusName: String, pubYear: String = "2021", editors: List[String]=List(),
                        tl2: String="The Letters as Loot / Brieven als Buit-corpus", addMeta: Boolean = true)
{
  def makeInterp(k: String, v: String) = <interpGrp type={k}><interp>{v}</interp></interpGrp>


  def getInterp(b: Elem, key: String) = (b \\ "interpGrp")
    .filter(x => (x \ "@type").text == key)
    .map(x => (x \ "interp" \ "@value").text)
    .filter(_.nonEmpty).mkString("; ")

  implicit def map(s: String) : Map[String,String] => String = m => m(s)
  def constant(s: String)(m: Map[String,String]) = s
  def makePid(m: Map[String,String]) : String = "INT_" + m("pid") + "_tei_unannotated"

  val level = 1

  val obviousMapping:Map[String,  Map[String,String] => String] = Map(
    s"titleLevel${level}" -> "title",
    "titleLevel2" -> constant(tl2),
   // "editorLevel2" -> constant("Marijke van der Wal (Programme leader), Gijsbert Rutten, Judith Nobels and Tanja Simons"),
    s"authorLevel${level}" -> "author",
    s"witnessYearLevel${level}_from" -> s"witnessYear_from",
    s"witnessYearLevel${level}_to" -> s"witnessYear_to" ,
    s"textYearLevel${level}_from" ->  s"witnessYear_from",
    s"textYearLevel${level}_to" -> s"witnessYear_to",
    s"pubYearLevel2_from" ->  constant(pubYear),
    s"pubYearLevel2_to" -> constant(pubYear),
    //s"titleLevel3" -> constant(corpusName),
    s"corpusProvenance" -> constant(corpusName),
    //s"witnessYearLevel3" -> "seriesYears",
    //s"pubYear_from" -> "yearOfPublicationMin",
    //s"pubYear_to" -> "yearOfPublicationMax",
    "primaryLanguage" -> constant("nl"),
    "translation" ->  constant("false"),
    "pid" -> makePid,
    "sourceID" -> "pid"
  )

  def intMetadataP(properties: Map[String,String]) = {
    //val id = properties("nederlabID")
    /*
    val persons = getAuthors(id)
    val personXML = authorsToXML(persons)

    val genre = getGenre(id)
    val genreXML = genre.map(g => interp("genre", g))

    val localization = getLocalization(id)
    val locXML = localization.flatMap(m => m.map{case (k,v) => interp(k,v)})

    val pubplace = getPubplace(id)
    val pubXML =  pubplace.flatMap(m => m.map{case (k,v) => interp(k,v)})
    */
    val obvious = obviousMapping.map{case (k,v) => k -> v(properties)}.map{case (k,v) => makeInterp(k,v)}
    obvious ++ <interpGrp type="editorLevel2">{editors.map(ed => <interp>{ed}</interp>)}</interpGrp>//  ++ personXML ++ genreXML ++ locXML ++ pubXML
  }

  def intMetadata(b: Elem): Elem = {
    val namez = (b \\ "interpGrp").map(x => (x \ "@type").text).toSet
    val props = namez.map(n => n -> getInterp(b,n)).toMap

    val newMeta = intMetadataP(props)
    <listBibl xml:id="inlMetadata"><bibl>{newMeta}</bibl></listBibl>
  }


  def wolkenVersie(d: Elem) = {
    val d0 = stripAnnotation.stripAnnotation(d).asInstanceOf[Elem]

    val d1 = if (addMeta) {
      val intMeta = intMetadata((d0 \\ "listBibl").head.asInstanceOf[Elem])
      PostProcessXML.updateElement5(d0, _.label == "listBibl", b => {
        val b0 = <listBibl type="BabMetadata">
          {b.child}
        </listBibl>
        val b1 = intMetadata(b0)
        Seq(b1, b0)
      }).asInstanceOf[Elem]
    } else d0
    d1.asInstanceOf[Elem]
  }

  // voor BaB:

  // voor aanvullingen /mnt/Projecten/Corpora/Historische_Corpora/BrievenAlsBuitAanvulling/Versie_1.0  /mnt/Projecten/Corpora/Historische_Corpora/Nederlab/Wolkencorpus/BaBAanvulling

  val aanvullingSource = "/mnt/Projecten/Corpora/Historische_Corpora/BrievenAlsBuitAanvulling/Versie_1.0"
  val aanvullingTarget = "/mnt/Projecten/Corpora/Historische_Corpora/Nederlab/Wolkencorpus/BaBAanvulling"

  def main(args: Array[String]): Unit = {
    ProcessFolder.processFolder(new File(source), new File(target), (in,out) => {
      val d = XML.load(in)
      XML.save(out, wolkenVersie(d), "UTF-8")
    })
  }
}

object doBaB {
  val babSource="/mnt/Projecten/Corpora/Historische_Corpora/BrievenAlsBuit/2.8TDN"
  val babTarget="/mnt/Projecten/Corpora/Historische_Corpora/Nederlab/Wolkencorpus/BaB"
  val w = wolkenCorpus(babSource, babTarget, "Brieven als Buit", "2020",
    tl2="The Letters as Loot / Brieven als Buit-corpus",
    editors = List("Marijke van der Wal", "Gijsbert Rutten", "Judith Nobels", "Tanja Simons"))

  def main(args: Array[String]): Unit = {
    w.main(Array())
  }
}


object doBaBaanvulling {
  val aanvullingSource = "/mnt/Projecten/Corpora/Historische_Corpora/BrievenAlsBuitAanvulling/Versie_1.0"
  val aanvullingTarget = "/mnt/Projecten/Corpora/Historische_Corpora/Nederlab/Wolkencorpus/BaBAanvulling"
  val w = wolkenCorpus(aanvullingSource, aanvullingTarget, "Brieven als Buit-2", "2021",
    editors = List("Marijke van der Wal"),
    tl2="Brieven als Buit-2/Letters as Loot-2")

  def main(args: Array[String]): Unit = {
    w.main(Array())
  }
}

object doGysseling {
  val gysSource = "/mnt/Projecten/Corpora/Historische_Corpora/CorpusGysseling/TeIndexeren/2020_07_31/"
  val  gysTarget = "/mnt/Projecten/Corpora/Historische_Corpora/Nederlab/Wolkencorpus/CorpusGysseling/"
  val w = wolkenCorpus(gysSource, gysTarget, "Corpus Gysseling", "2021",
    editors = List("Gys"),
    tl2="Gys", addMeta = false)

  def main(args: Array[String]): Unit = {
    w.main(Array())
  }
}

object aBibl {
  val marijke_voorbeeld =       <listBibl id="inlMetadata">
    <bibl>
      <interpGrp type="title">
        <interp value="To Tobias de Groot, 16 november 1672"/>
      </interpGrp>
      <interpGrp type="author">
        <interp value="Belijtje Juriaans"/>
      </interpGrp>
      <interpGrp type="witnessYear_from">
        <interp value="1672"/>
      </interpGrp>
      <interpGrp type="witnessYear_to">
        <interp value="1672"/>
      </interpGrp>
      <interpGrp type="aantal_paginas">
        <interp value="2"/>
      </interpGrp>
      <interpGrp type="aantal_woorden">
        <interp value="638"/>
      </interpGrp>
      <interpGrp type="adr_beroep">
        <interp value="korporaal"/>
      </interpGrp>
      <interpGrp type="adr_bijzonderheden">
        <interp value=""/>
      </interpGrp>
      <interpGrp type="adr_geb_decennium">
        <interp value="unknown"/>
      </interpGrp>
      <interpGrp type="adr_geb_jaar">
        <interp value="unknown"/>
      </interpGrp>
      <interpGrp type="adr_geb_lftcat">
        <interp value=""/>
      </interpGrp>
      <interpGrp type="adr_geb_plaats">
        <interp value=""/>
      </interpGrp>
      <interpGrp type="adr_geslacht">
        <interp value="man"/>
      </interpGrp>
      <interpGrp type="adr_godsdienst">
        <interp value=""/>
      </interpGrp>
      <interpGrp type="adr_klasse">
        <interp value="midden-laag"/>
      </interpGrp>
      <interpGrp type="adr_loc_land">
        <interp value=""/>
      </interpGrp>
      <interpGrp type="adr_loc_land_norm">
        <interp value="Indonesi&#xEB;"/>
      </interpGrp>
      <interpGrp type="adr_loc_plaats">
        <interp value="batauia"/>
      </interpGrp>
      <interpGrp type="adr_loc_plaats_norm">
        <interp value="Batavia"/>
      </interpGrp>
      <interpGrp type="adr_loc_regio">
        <interp value=""/>
      </interpGrp>
      <interpGrp type="adr_loc_regio_norm">
        <interp value="Azi&#xEB;"/>
      </interpGrp>
      <interpGrp type="adr_loc_schip">
        <interp value=""/>
      </interpGrp>
      <interpGrp type="adr_loc_schip_norm">
        <interp value=""/>
      </interpGrp>
      <interpGrp type="adr_loc_straat">
        <interp value="jnt hamborger wapen"/>
      </interpGrp>
      <interpGrp type="adr_loc_straat_norm">
        <interp value="in het Hamburger wapen (herberg)"/>
      </interpGrp>
      <interpGrp type="adr_meer_pers_extra">
        <interp value=""/>
      </interpGrp>
      <interpGrp type="adr_meer_personen">
        <interp value="n"/>
      </interpGrp>
      <interpGrp type="adr_naam">
        <interp value="tobijas de / die groot / tobijas groot"/>
      </interpGrp>
      <interpGrp type="adr_naam_norm">
        <interp value="Tobias de Groot"/>
      </interpGrp>
      <interpGrp type="adr_opleiding">
        <interp value=""/>
      </interpGrp>
      <interpGrp type="adr_rel_tot_afz">
        <interp value=""/>
      </interpGrp>
      <interpGrp type="adr_rel_tot_afz_a">
        <interp value="17"/>
      </interpGrp>
      <interpGrp type="adr_rel_tot_afz_anders">
        <interp value=""/>
      </interpGrp>
      <interpGrp type="adr_rel_tot_afz_b">
        <interp value="0"/>
      </interpGrp>
      <interpGrp type="adr_soc_klimmer">
        <interp value="n"/>
      </interpGrp>
      <interpGrp type="afz_beroep">
        <interp value=""/>
      </interpGrp>
      <interpGrp type="afz_beroepsschrijver">
        <interp value="n"/>
      </interpGrp>
      <interpGrp type="afz_bijzonderheden">
        <interp value=""/>
      </interpGrp>
      <interpGrp type="afz_geb_decennium">
        <interp value="1640"/>
      </interpGrp>
      <interpGrp type="afz_geb_jaar">
        <interp value="unknown"/>
      </interpGrp>
      <interpGrp type="afz_geb_lftcat">
        <interp value=""/>
      </interpGrp>
      <interpGrp type="afz_geb_plaats">
        <interp value=""/>
      </interpGrp>
      <interpGrp type="afz_geslacht">
        <interp value="female"/>
      </interpGrp>
      <interpGrp type="afz_godsdienst">
        <interp value=""/>
      </interpGrp>
      <interpGrp type="afz_klasse">
        <interp value="middle-low"/>
      </interpGrp>
      <interpGrp type="afz_loc_land">
        <interp value=""/>
      </interpGrp>
      <interpGrp type="afz_loc_land_norm">
        <interp value="Nederland"/>
      </interpGrp>
      <interpGrp type="afz_loc_plaats">
        <interp value=""/>
      </interpGrp>
      <interpGrp type="afz_loc_plaats_norm">
        <interp value="Hoorn"/>
      </interpGrp>
      <interpGrp type="afz_loc_regio">
        <interp value=""/>
      </interpGrp>
      <interpGrp type="afz_loc_regio_norm">
        <interp value="Noord-Holland"/>
      </interpGrp>
      <interpGrp type="afz_loc_schip">
        <interp value=""/>
      </interpGrp>
      <interpGrp type="afz_loc_schip_norm">
        <interp value=""/>
      </interpGrp>
      <interpGrp type="afz_loc_straat">
        <interp value=""/>
      </interpGrp>
      <interpGrp type="afz_loc_straat_norm">
        <interp value=""/>
      </interpGrp>
      <interpGrp type="afz_meer_pers_extra">
        <interp value=""/>
      </interpGrp>
      <interpGrp type="afz_meer_personen">
        <interp value="n"/>
      </interpGrp>
      <interpGrp type="afz_naam">
        <interp value="beelijte / belijte juerians"/>
      </interpGrp>
      <interpGrp type="afz_naam_norm">
        <interp value="Belijtje Juriaans"/>
      </interpGrp>
      <interpGrp type="afz_opleiding">
        <interp value=""/>
      </interpGrp>
      <interpGrp type="afz_rel_tot_adr">
        <interp value="niece/cousin"/>
      </interpGrp>
      <interpGrp type="afz_rel_tot_adr_a">
        <interp value="18"/>
      </interpGrp>
      <interpGrp type="afz_rel_tot_adr_anders">
        <interp value=""/>
      </interpGrp>
      <interpGrp type="afz_rel_tot_adr_b">
        <interp value="0"/>
      </interpGrp>
      <interpGrp type="afz_soc_klimmer">
        <interp value="n"/>
      </interpGrp>
      <interpGrp type="autograaf">
        <interp value="non-autograph"/>
      </interpGrp>
      <interpGrp type="bijzonderheden">
        <interp value=""/>
      </interpGrp>
      <interpGrp type="code">
        <interp value="961"/>
      </interpGrp>
      <interpGrp type="cor_bijzonderheden">
        <interp value=""/>
      </interpGrp>
      <interpGrp type="cor_transcribent">
        <interp value=""/>
      </interpGrp>
      <interpGrp type="corpus">
        <interp value="1"/>
      </interpGrp>
      <interpGrp type="datum_dag">
        <interp value="16"/>
      </interpGrp>
      <interpGrp type="datum_gemaakt">
        <interp value="2009-10-07 04:10:18"/>
      </interpGrp>
      <interpGrp type="datum_jaar">
        <interp value="1672"/>
      </interpGrp>
      <interpGrp type="datum_maand">
        <interp value="11"/>
      </interpGrp>
      <interpGrp type="datum_onduidelijk">
        <interp value="n"/>
      </interpGrp>
      <interpGrp type="gen_familie">
        <interp value=""/>
      </interpGrp>
      <interpGrp type="gen_gebeurtenissen">
        <interp value="Verzuchtingen over de oorlog, gelardeerd met een psalmfragment. Het gaat niet goed met de oorlog. De Fransen hebben Utrecht ingenomen. Oranje is stadhouder geworden en de regenten zullen boeten voor hun landverraad. Brandenburgse en L&#xFC;nenburgse troepen zijn al in Westfalen, maar helaas nog niet in de Republiek. \nBurgemeester Langeweagen is bankroet en zijn huis verwoest. Op zijn hoofd is 600 gulden gezet."/>
      </interpGrp>
      <interpGrp type="gen_nietNL_extra">
        <interp value=""/>
      </interpGrp>
      <interpGrp type="gen_nietNLwoorden">
        <interp value="n"/>
      </interpGrp>
      <interpGrp type="gen_personen">
        <interp value="Burgermeester Langewagen."/>
      </interpGrp>
      <interpGrp type="gen_plaatsen">
        <interp value="Maastricht"/>
      </interpGrp>
      <interpGrp type="gen_schepen">
        <interp value=""/>
      </interpGrp>
      <interpGrp type="handschrift">
        <interp value="gemiddeld"/>
      </interpGrp>
      <interpGrp type="pid">
        <interp value="bab0961"/>
      </interpGrp>
      <interpGrp type="plaats_adres">
        <interp value="brief"/>
      </interpGrp>
      <interpGrp type="regiocode">
        <interp value="Noord-Holland (excluding Amsterdam)"/>
      </interpGrp>
      <interpGrp type="relatie_a">
        <interp value="niece/cousin"/>
      </interpGrp>
      <interpGrp type="relatie_b">
        <interp value="unknown"/>
      </interpGrp>
      <interpGrp type="signatuur">
        <interp value="HCA 30-223"/>
      </interpGrp>
      <interpGrp type="status">
        <interp value="6"/>
      </interpGrp>
      <interpGrp type="subcorpus">
        <interp value="17B"/>
      </interpGrp>
      <interpGrp type="trans_bestand">
        <interp value="17-06-2009 236-238-TR-def"/>
      </interpGrp>
      <interpGrp type="trans_datum">
        <interp value="2009-10-12 00:00:00"/>
      </interpGrp>
      <interpGrp type="type_brief">
        <interp value="private"/>
      </interpGrp>
      <interpGrp type="images">
        <interp value="INL-17deE-gecropt-foto's BaB/17-06-2009_236.jpg"/>
        <interp value="INL-17deE-gecropt-foto's BaB/17-06-2009_237.jpg"/>
        <interp value="./Knip/17-06-2009_237.geknipt.png"/>
        <interp value="INL-17deE-gecropt-foto's BaB/17-06-2009_238.jpg"/>
      </interpGrp>
    </bibl>
  </listBibl>
}