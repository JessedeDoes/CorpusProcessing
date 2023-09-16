package corpusprocessing.GCND
import scala.xml._
import database.DatabaseUtilities.Select
import database._
import org.json4s._
import org.json4s.jackson.Serialization._
import scala.xml.PrettyPrinter
import corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.conll_u.{AlpinoSentence, AlpinoToken}
import java.io.PrintWriter


/*
Problemen:
- foutmeldingen bij UD conversie - tokens zonder makkelijk vindbaar head
/home/jesse/workspace/XmlToRdf/data/GCND/Alpino/speech.alpino.100.xml
  sentence ID: H016p_3--H016p_3_1--0315
  error: No internal head for 13:voo
    in internalHeadPosition
        node -- id:22  begin:13  end:22  cat:ti  rel:body
    in internalHeadPositionWithGapping
        node -- id:22  begin:13  end:22  cat:ti  rel:body
    in externalHeadPosition
        node -- id:21  begin:12  end:13  word:voo  pt:vz  rel:cmp  lemma:om
    in addDependencyRelations
        node -- id:21  begin:12  end:13  word:voo  pt:vz  rel:cmp  lemma:om
/home/jesse/workspace/XmlToRdf/data/GCND/Alpino/speech.alpino.10.xml
  sentence ID: H016p_3--H016p_3_1--0487
  error: No external head for 9:hé
    in externalHeadPosition
        node -- id:12  begin:8  end:9  word:hé  pt:tsw  rel:tag  lemma:hé
    in addDependencyRelations
        node -- id:12  begin:8  end:9  word:hé  pt:tsw  rel:tag  lemma:hé

- lemmatisering bij instructies als [ @alt om voor ], je krijgt dan lemma 'om' bij 'voor'

- elan_annotatie zonder tekst_lv

gcnd=# select count(*) from elan_annotatie where tekst_lv is null;
 count
-------
    94
(1 row)

- lichte en zware vervlaamsing moeilijk te aligneren

De Alpino analyses zijn niet helemaal volgens de Lassy guidelines:


<node begin="8" cat="np" end="10" id="7" rel="su">
    <node isWord="true" end="9" begin="5" buiging="zonder" graad="basis" id="8" lemma="weinig" naamval="stan" npagr="agr" pdtype="grad" pos="adj" positie="prenom" postag="VNW(onbep,grad,stan,prenom,zonder,agr,basis)" pt="vnw" rel="hd" root="weinig" sense="weinig" vwtype="onbep" word="weinig"/>
    <node isWord="true" end="10" begin="6" getal="mv" graad="basis" id="9" lemma="schip" ntype="soort" pos="noun" postag="N(soort,mv,basis)" pt="n" rel="hd" root="schip" sense="schip" word="schepen"/>
</node>


Weinig zou een det relatie met schip moeten hebben.
Zo komen weinig en schip er allebei als "su" uit in de pure dependenties

 */

object GCNDDatabase {
  lazy val pretty = new PrettyPrinter(1000,4)
  val config = new Configuration(name="gcnd", server="svowdb20.ivdnt.loc", user="postgres", password="inl", database = "gcnd")
  val db = new Database(config)

  implicit lazy val serializationFormats: Formats = DefaultFormats

  case class Token(text_zv: String, text_lv: String)

  lazy val elanQ = Select(r => ElanAnnotation(
    r.getInt("elan_annotatie_id"),
    r.getInt("transcriptie_id"),
    r.getString("annotatie_code"),
    r.getInt("opname__persoon_id"),
    r.getString("tekst_lv"),
    r.getString("tekst_zv"),
    r.getInt("starttijd"),
    r.getInt("eindtijd"),
  ), "elan_annotatie")

  lazy val alpinoQ = Select(
    r => AlpinoAnnotation(
      r.getInt("alpino_annotatie_id"),
      r.getInt("transcriptie_id"),
      r.getString("annotatie_code"),
      r.getInt("opname__persoon_id"),
      r.getString("tekst_lv"),
      r.getString("tekst_zv"),
      r.getString("alpino_xml"),
      r.getString("tokens"),
      r.getInt("starttijd"),
      r.getInt("eindtijd")), "alpino_annotatie")

  lazy val alpinos: Seq[AlpinoAnnotation] = db.slurp(alpinoQ).sortBy(x => x.sortKey)
  lazy val elans: Seq[ElanAnnotation] = db.slurp(elanQ).sortBy(_.starttijd)

  def getAlpinoAnnotations(transcriptie_id: Int): Seq[AlpinoAnnotation] = {
    val q = alpinoQ.copy(from= s"alpino_annotatie where transcriptie_id=$transcriptie_id")
    db.slurp(q).sortBy(x => x.sortKey)
  }

  def getElanAnnotations(transcriptie_id: Int): Seq[ElanAnnotation] = {
    val q = elanQ.copy(from = s"elan_annotatie where transcriptie_id=$transcriptie_id")
    db.slurp(q).sortBy(x => x.starttijd + x.eindtijd)
  }

  def getPseudoTEI(transcriptie_id: Int) = <TEI>
    <teiHeader/>
    <text>
    <body><div>{getAlpinoAnnotations(transcriptie_id).map(_.TEI.pseudoTEI)}</div></body>
    </text>
  </TEI>

  def getPseudoFoLiAForAlpinoAnnotations(transcriptie_id: Int) =
    <FoLiA xml:id={"gcnd.transcriptie." + transcriptie_id} version="2.5.1" xmlns:folia="http://ilk.uvt.nl/folia" xmlns="http://ilk.uvt.nl/folia">
    <metadata  type="internal" xmlns="http://ilk.uvt.nl/folia">
      <annotations>
        <pos-annotation set="hdl:1839/00-SCHM-0000-0000-000B-9"/>
        <lemma-annotation set="hdl:1839/00-SCHM-0000-0000-000E-3"/>
        <division-annotation set="gcnd_div_classes"/>
        <timesegment-annotation set="cgn"/>
        <text-annotation set="https://raw.githubusercontent.com/proycon/folia/master/setdefinitions/text.foliaset.ttl"/>
        <token-annotation/>
        <sentence-annotation/>
        <syntax-annotation set="gcnd.syntax"/>
        <dependency-annotation set="gcnd.dependency"/>
      </annotations>
      <foreign-data>
        {Metadata.getMetadata(transcriptie_id)}
      </foreign-data>
    </metadata>{getAlpinoAnnotations(transcriptie_id).sortBy(_.sortKey).map(x => x.Folia.pseudoFolia(true))}
  </FoLiA>

  def getPseudoFoLiAForElanAnnotations(transcriptie_id: Int) =
    <FoLiA xml:id={"gcnd.transcriptie." + transcriptie_id} version="2.5.1" xmlns:folia="http://ilk.uvt.nl/folia" xmlns="http://ilk.uvt.nl/folia">
      <metadata type="internal" xmlns="http://ilk.uvt.nl/folia">
        <annotations>
          <pos-annotation set="hdl:1839/00-SCHM-0000-0000-000B-9"/>
          <lemma-annotation set="hdl:1839/00-SCHM-0000-0000-000E-3"/>
          <syntax-annotation set="lassy.syntax.annotation"/>
          <syntax-annotation set="ud.syntax.annotation"/>
          <division-annotation set="gcnd_div_classes"/>
          <timesegment-annotation set="cgn"/>
          <text-annotation set="https://raw.githubusercontent.com/proycon/folia/master/setdefinitions/text.foliaset.ttl"/>
          <token-annotation/>
          <sentence-annotation set="gcnd.sentence"/>
          <dependency-annotation set="gcnd.dependency"/>
        </annotations>
        <foreign-data>
          {Metadata.getMetadata(transcriptie_id)}
        </foreign-data>
      </metadata>{getElanAnnotations(transcriptie_id).sortBy(_.starttijd).map(x => x.pseudoFolia())}
    </FoLiA>

  def getId(n: Node): String = n.attributes.filter(a => a.prefixedKey.endsWith(":id") ||
    a.key.equals("id")).map(a => a.value.toString).head

  def main(args: Array[String])  = {
    if (false) {
      val out = new PrintWriter("/tmp/gcnd.test.tei.xml")
      out.println(pretty.format(getPseudoTEI(1)))
      out.close()
    }

    val foliaWithAlpino = getPseudoFoLiAForAlpinoAnnotations(1)

    val alpinoDumpDir = new java.io.File("data/GCND/Alpino/")
    alpinoDumpDir.mkdir()


    (foliaWithAlpino \\ "speech").foreach(a => {
      val e = a.asInstanceOf[Elem]
      val speech_id = getId(e)
      val alpinos = (e \\ "alpino_ds");
      val alpino = alpinos.head.asInstanceOf[Elem]
      //.asInstanceOf[Elem]
      // println("Aantal alpinos: " + alpinos.size)
      XML.save(alpinoDumpDir.getCanonicalPath + "/" + speech_id + ".xml", alpino)
    })

    val out1 = new PrintWriter("data/GCND/gcnd.test.folia.xml")
    out1.println(pretty.format(foliaWithAlpino))
    out1.close()

    val out2 = new PrintWriter("data/GCND/gcnd.test.folia.elans.xml")
    out2.println(pretty.format(getPseudoFoLiAForElanAnnotations(1)))
    out2.close()
    println("Nopes:" + ElanStats.nopes  + " nulls: " + ElanStats.nulls)
  }
}


/*

gcnd=# \d alpino_annotatie;
                                                Table "public.alpino_annotatie"
       Column        |       Type        | Collation | Nullable |                            Default
---------------------+-------------------+-----------+----------+---------------------------------------------------------------
 alpino_annotatie_id | integer           |           | not null | nextval('alpino_annotatie_alpino_annotatie_id_seq'::regclass)
 transcriptie_id     | integer           |           | not null |
 annotatie_code      | character varying |           | not null |
 opname__persoon_id  | integer           |           | not null |
 tekst_lv            | text              |           |          |
 tekst_zv            | text              |           | not null |
 alpino_xml          | text              |           | not null |
 tokens              | text              |           |          |
 starttijd           | integer           |           | not null |
 eindtijd            | integer           |           | not null |

Column       |       Type        | Collation | Nullable |                          Default
--------------------+-------------------+-----------+----------+-----------------------------------------------------------
elan_annotatie_id  | integer           |           | not null | nextval('elan_annotatie_elan_annotatie_id_seq'::regclass)
transcriptie_id    | integer           |           | not null |
annotatie_code     | character varying |           | not null |
opname__persoon_id | integer           |           |          |
tekst_lv           | text              |           |          |
tekst_zv           | text              |           | not null |
starttijd          | integer           |           | not null |
eindtijd           | integer           |           | not null |


 */