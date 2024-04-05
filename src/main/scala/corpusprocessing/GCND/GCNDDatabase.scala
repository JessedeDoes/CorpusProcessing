package corpusprocessing.GCND
import database.DatabaseUtilities.{Select, doeHet}
import database._
import org.json4s._

import java.io.PrintWriter
import scala.xml._


object Preparation {
  val queries =
    """start transaction;
      |drop view if exists elan_2_alpino cascade;
      |drop view if exists elan_2_alpino_unique;
      |create view elan_2_alpino as
      |
      |	select e.transcriptie_id, elan_annotatie_id, alpino_annotatie_id,
      |	case
      |	  when a.starttijd >= e.starttijd and a.starttijd < e.eindtijd then least(e.eindtijd,a.eindtijd) - a.starttijd
      |	  when a.eindtijd > e.starttijd and a.eindtijd <= e.eindtijd then a.eindtijd - greatest(a.starttijd, e.starttijd)
      |	  else 0
      |	end as overlap
      |	from elan_annotatie e, alpino_annotatie a
      |	where e.transcriptie_id = a.transcriptie_id and
      |	((a.starttijd >= e.starttijd and a.starttijd < e.eindtijd) or (a.eindtijd > e.starttijd and a.eindtijd <= e.eindtijd));
      |
      |	create view alpino_2_elan_unique as select
      |		transcriptie_id,
      |		alpino_annotatie_id,
      |		(array_agg(elan_annotatie_id order by overlap desc))[1] as elan_annotatie_id,
      |		array_agg(elan_annotatie_id order by overlap desc) as options,
      |		array_agg(overlap order by overlap desc) as overlaps from elan_2_alpino group by alpino_annotatie_id, transcriptie_id;
      | create table if not exists tagged_tokens (elan_annotatie_id integer, tokens text);
      | drop view elan_annotatie_plus;
      | create view  elan_annotatie_plus as select elan_annotatie.*, tagged_tokens.tokens as tagged_tokens from elan_annotatie left join tagged_tokens on tagged_tokens.elan_annotatie_id=elan_annotatie.elan_annotatie_id;
      |commit;
      |""".stripMargin.split(";")
}
object GCNDDatabase {
  val doAll = false
  val maxTranscriptions = if (doAll) Integer.MAX_VALUE else 30
  lazy val pretty = new PrettyPrinter(100,4)
  val config = new Configuration(name="gcnd.nogmaals", server="svowdb20.ivdnt.loc", user="postgres", password="inl", database = "gcnd")
  val onefile_config =new Configuration(name="gcnd.nogmaals", server="svowdb20.ivdnt.loc", user="postgres", password="inl", database = "gcnd_prev")

  val db = new Database(config)
  Preparation.queries.foreach(db.runStatement)

  implicit lazy val serializationFormats: Formats = DefaultFormats

  val transcriptionQ = Select(
    r => r.getInt("transcriptie_id"), "transcriptie"
  )
  lazy val elan_transcriptie_ids = db.slurp(transcriptionQ)
  lazy val transcriptions = elan_transcriptie_ids.iterator.map(Transcription)

  def getId(n: Node): String = n.attributes.filter(a => a.prefixedKey.endsWith(":id") ||
    a.key.equals("id")).map(a => a.value.toString).head

  lazy val aludErrorMap: Map[String, String] = {
    import corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.grouping.groupWithFirst
    val lines = scala.io.Source.fromFile("data/GCND/alud.errors").getLines().toList
    groupWithFirst[String](lines, x => x.startsWith("data")).filter(x => x.head.startsWith("data")).map(g => g.head.replaceAll(".*/", "").replaceAll(".xml$", "") -> g.tail.mkString("\n")).toMap
  }

  def saveAlpinoParses(transcriptie: Transcription, alpinoDumpDir: java.io.File) = {
    transcriptie.alpinoAnnotations.foreach(a => {
      val parse = fixTagWithoutNucl.fixTagWithoutNucl(a.alpinoParseAsXML)
      XML.save(alpinoDumpDir.getCanonicalPath + "/" + a.sentenceId + ".xml", parse)
    })
  }

  val foliadDir = "data/GCND/Folia/"

  def emptyDir(d: String)  = {
    val dir = new java.io.File(d)
    dir.listFiles.filter(x => x.isFile).foreach(_.delete())
  }

  def createFolia(transcription: Transcription, outputFilenameTemplate: String = foliadDir  + "gcnd.#.folia.xml"): Unit = {

    val outputFilename = outputFilenameTemplate.replaceAll("#", transcription.transcriptie_id.toString)
    val out2 = new PrintWriter(outputFilename)
    val e = transcription.pseudoFoLiAForElanAnnotations
    // XML.write(out2, e, enc="UTF-8", doctype = DocType("FoLiA"), xmlDecl = true)
    out2.println(pretty.format(e))
    out2.close()
  }

  def createTEI() = {
    val out = new PrintWriter("/tmp/gcnd.test.tei.xml")
    // out.println(pretty.format(getPseudoTEI(1)))
    out.close()
  }

  def add(a: Map[String,Any], b: Map[String,Any])  = {
    val keys = a.keySet ++ b.keySet
    keys.map(k => {
      val va = a(k)
      val vb = b(k)
      val vc = if (k == "transcriptie_id")
        "id";
      else (va.toString.toInt + vb.toString.toInt)
      k -> vc
    }).toMap
  }

  def main(args: Array[String])  = {



    val pw = new PrintWriter("/tmp/gcnd.transcripts.log")
    val pw1 = new PrintWriter("/tmp/gcnd.log")

    emptyDir(foliadDir)

    val abouts = transcriptions.take(maxTranscriptions).map(
     x => {
       createFolia(x)
       pw.println(x.about)
       x.about
     }
   )
    val about = abouts.reduce(add)
    about.toList.sortBy(_._1).foreach({case (k,v)  => pw1.println(s"$k\t$v")})
    pw.close()
    pw1.close()
    println("Nopes:" + ElanStats.nopes  + " nulls: " + ElanStats.nulls)
  }
}

object dumpAlpinoParses {
  def main(args: Array[String])  = {

      val alpinoDumpDir = new java.io.File("/home/jesse/Downloads/AlpinoDumps/")
      alpinoDumpDir.mkdir()
      GCNDDatabase.transcriptions.foreach(t => GCNDDatabase.saveAlpinoParses(t, alpinoDumpDir))
      // saveAlpinoParses(transcriptie_id = 1, alpinoDumpDir)

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


// lazy val alpinoQ: AlmostQuery[AlpinoAnnotation] = doeHet[AlpinoAnnotation](alpinoQ0)
// lazy val alpinos: Seq[AlpinoAnnotation] = db.slurp(alpinoQ).sortBy(x => x.sortKey)



// lazy val elans: Seq[ElanAnnotation] = db.slurp(elanQ).sortBy(_.starttijd)





/*

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

*/

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

De Alpino analyses zijn niet helemaal volgens de Lassy guidelines: (2x hd bijvoorbeeld)


<node begin="8" cat="np" end="10" id="7" rel="su">
    <node isWord="true" end="9" begin="5" buiging="zonder" graad="basis" id="8" lemma="weinig" naamval="stan" npagr="agr" pdtype="grad" pos="adj" positie="prenom" postag="VNW(onbep,grad,stan,prenom,zonder,agr,basis)" pt="vnw" rel="hd" root="weinig" sense="weinig" vwtype="onbep" word="weinig"/>
    <node isWord="true" end="10" begin="6" getal="mv" graad="basis" id="9" lemma="schip" ntype="soort" pos="noun" postag="N(soort,mv,basis)" pt="n" rel="hd" root="schip" sense="schip" word="schepen"/>
</node>


Weinig zou een det relatie met schip moeten hebben.
Zo komen weinig en schip er allebei als "su" uit in de pure dependenties

 */
