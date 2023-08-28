package corpusprocessing.GCND
import database.DatabaseUtilities.Select
import database._
import org.json4s._
import org.json4s.jackson.Serialization._
import scala.xml.PrettyPrinter
import corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.conll_u.{AlpinoSentence, AlpinoToken}
import java.io.PrintWriter
import scala.xml._
import GCNDDatabase.{Token, elans}
case class AlpinoAnnotation(alpino_annotatie_id: Int,
                            transcriptie_id: Int,
                            annotatie_code: String,
                            opname_persoon_id: Int,
                            tekst_lv: String,
                            tekst_zv: String,
                            alpino_xml: String,
                            tokens: String,
                            starttijd: Int,
                            eindtijd: Int)
{
  implicit lazy val serializationFormats: Formats = DefaultFormats

  lazy val x = alpino_xml.replaceAll("^b'", "").replaceAll("'$", "").replaceAll("\\\\n", "\n")
  lazy val parse: Elem = XML.loadString(x)
  lazy val sentence = AlpinoSentence(parse)
  lazy val alpinoTokens = sentence.alpinoTokens
  lazy val alignedTokens: List[Token] = read[Array[Token]](tokens).toList

  lazy val pseudoTEI = {
    <u start={this.starttijd.toString} end={this.eindtijd.toString} xml:id={alpino_annotatie_id.toString}>
      {Metadata.getMetadata(this)}
      <seg type="alpinoInput">
        {sentence.input_transcript}
      </seg>{overLappingElanAnnotations.map(e =>
      <seg type="elanLichteVernederlandsing" start={e.starttijd.toString} end={e.eindtijd.toString}>
        {e.tekst_lv}
      </seg>
        <seg type="elanZwareVernederlandsing" start={e.starttijd.toString} end={e.eindtijd.toString}>
          {e.tekst_zv}
        </seg>
    )}<seg type="alpinoLichteVernederlandsing">
      {alignedTokens.map(t => t.text_lv).mkString(" ")}
    </seg>
      <seg type="alpinoZwareVernederlandsing">
        {alignedTokens.map(t => t.text_zv).mkString(" ")}
      </seg>
      <seg type="verrijkteZin">
        <s>
          {if (alignedTokens.size == alpinoTokens.size) {
          val zipped: Seq[(Token, AlpinoToken)] = alignedTokens.zip(alpinoTokens)
          val danges = zipped.map({ case (t, a) => <w norm={t.text_zv} lemma={a.lemma} pos={a.postag}>
            {t.text_lv}
          </w>
          })
          danges
        } else Seq()}
        </s>
      </seg>
    </u>
  }

  lazy val overLappingElanAnnotations = elans.filter(e =>
    e.starttijd >= starttijd & e.starttijd <= eindtijd || e.eindtijd >= starttijd & e.eindtijd <= eindtijd
  )
}
