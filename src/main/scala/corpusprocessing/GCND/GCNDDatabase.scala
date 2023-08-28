package corpusprocessing.GCND
import scala.xml._
import database.DatabaseUtilities.Select
import database._
import org.json4s._
import org.json4s.jackson.Serialization._
import corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.conll_u.{AlpinoSentence, AlpinoToken}
object GCNDDatabase {
   val config = new Configuration(name="gcnd", server="svowdb20.ivdnt.loc", user="postgres", password="inl", database = "gcnd")
   val db = new Database(config)
  implicit lazy val serializationFormats: Formats = DefaultFormats
  case class AlpinoAnnotation(alpino_annotatie_id: Int, transcriptie_id: Int, annotatie_code: String, opname_persoon_id: Int, tekst_lv: String, tekst_zv: String, alpino_xml: String, tokens: String, starttijd: Int, eindtijd: Int) {
    lazy val x = alpino_xml.replaceAll("^b'", "").replaceAll("'$","").replaceAll("\\\\n", "\n")
    lazy val parse: Elem = XML.loadString(x)
    lazy val sentence = AlpinoSentence(parse)
    lazy val alpinoTokens = sentence.alpinoTokens
    lazy val alignedTokens: List[Token] = read[Array[Token]](tokens).toList

    lazy val pseudoTEI = {
      <s>
        {
      if (alignedTokens.size == alpinoTokens.size) {
        val zipped: Seq[(Token, AlpinoToken)] = alignedTokens.zip(alpinoTokens)
        val danges = zipped.map({ case (t, a) => <w zv={t.text_zv} lv={t.text_lv} lemma={a.lemma} pos={a.postag}></w> })
        danges
      } else Seq() }
      </s>
    }
  }

  case class Token(text_zv: String, text_lv: String)
  case class ElanAnnotation(elan_annotatie_id: Int,
                            transcriptie_id: Int,
                            annotatie_code: String,
                            opname_persoon_id: Int,
                            tekst_lv: String,
                            tekst_zv: String,
                            starttijd: Int,
                            eindtijd: Int
                           )

  lazy val elanQ = Select(r => ElanAnnotation(
    r.getInt("elan_annotatie_id"),
    r.getInt("transcriptie_id"),
    r.getString("annotatie_code"),
    r.getInt("opname_persoon_id"),
    r.getString("tekst_lv"),
    r.getString("tekst_zv"),
    r.getInt("starttijd"),
    r.getInt("eindtijd"),
  ), "elan_annotaties")

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

  lazy val alpinos: Seq[AlpinoAnnotation] = db.slurp(alpinoQ)

  def main(args: Array[String])  = {
    alpinos.foreach(x => {
      println("##############")
      println(x.sentence.input_transcript)
      println(x.alignedTokens.size)
      println(x.alpinoTokens.size)
      println(x.pseudoTEI)
    })
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