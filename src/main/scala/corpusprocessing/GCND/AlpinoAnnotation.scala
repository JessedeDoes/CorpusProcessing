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
import posmapping.{CGNPoSTagging, CGNTagset}

import scala.collection.{AbstractSeq, immutable}

object Stuff {
  def formatTime (x: Int) = {
    val y = 60 * 60 * 1000
    val h = x / y
    val m = (x - (h * y)) / (y / 60)
    val s = (x - (h * y) - (m * (y / 60))) / 1000
    val mi = x - (h * y) - (m * (y / 60)) - (s * 1000)
    (h, m, s, mi)
    val hs = String.format("%02d", h.asInstanceOf[Object])
    val ms = String.format("%02d", m.asInstanceOf[Object])
    val ss = String.format("%02d", s.asInstanceOf[Object])
    val mis = String.format("%03d", mi.asInstanceOf[Object])
    s"$hs:$ms:$ss.$mis"
  }

  val alpinoScope = <alpino xmlns="http://alpino.fake.url"/>.scope
}

import Stuff._
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
  lazy val alpinoParseAsXML: Elem = XML.loadString(x)
  lazy val sentence = AlpinoSentence(alpinoParseAsXML)
  lazy val alpinoTokens: Seq[AlpinoToken] = sentence.alpinoTokens.zipWithIndex.map({case (x,i) => x.copy(id = Some(s"annotation.$alpino_annotatie_id.w.$i"))})
  lazy val alignedTokens: List[Token] = read[Array[Token]](tokens).toList
  lazy val zipped: Seq[(Token, AlpinoToken)] = alignedTokens.zip(alpinoTokens)
  val speech_id = s"speech.$alpino_annotatie_id"

  lazy val informativeT: Elem =  {  <info> <t class="alpinoInput">
        {sentence.input_transcript}
      </t>
   <t class="alpinoLightDutchification">
    {alignedTokens.map(t => t.text_lv).mkString(" ")}
   </t>
    <t class="alpinoHeavyDutchification">
      {alignedTokens.map(t => t.text_zv).mkString(" ")}
    </t>{overLappingElanAnnotations.map(e =>
      <div xml:id={speech_id + ".elan." + e.elan_annotatie_id} class="elanAnnotation" begintime={formatTime(e.starttijd)} endtime={formatTime(e.eindtijd)}>
        <t class="elanLightDutchification">
          {e.tekst_lv}
        </t>
        <t class="elanHeavyDutchification">
          {e.tekst_zv}
        </t>
      </div>
    )}
  </info> }

   def pseudoFolia(includeAlpinoParse: Boolean = false) = {

    <speech xml:id={speech_id}>
      {informativeT.child}
      <s xml:id={s"s.$alpino_annotatie_id"}>
        {if (alignedTokens.size == alpinoTokens.size) {

        val danges = zipped.map({ case (t, a) =>
          val posTag = CGNTagset.fromString(a.postag)
          <w xml:id={a.id.get}>
            <t class="heavyDutchification">{t.text_zv}</t>
            <t class="lightDutchification">{t.text_lv}</t>
            <lemma class={a.lemma}/>
            <pos class={a.postag} head={posTag.pos}>{posTag.features.map(f => {<feat class={f.value} subset={f.name}/>} )}</pos>
          </w>
        })
        danges
      } else Seq()}
        <timing>
          <timesegment begintime={formatTime(this.starttijd)} endtime={formatTime(this.eindtijd)}>
            {zipped.map({case (t,a) => <wref id={a.id.get} t={t.text_lv}/>})}
          </timesegment>

        </timing>
      </s>
      {if (includeAlpinoParse) <foreign-data>{alpinoParseAsXML.copy(scope = alpinoScope)}</foreign-data>}
    </speech>
  }
  //       {Metadata.getMetadata(this)}
  lazy val pseudoTEI = {
    <u start={this.starttijd.toString} end={this.eindtijd.toString} xml:id={alpino_annotatie_id.toString}>

      <seg type="alpinoInput">
        {sentence.input_transcript}
      </seg>{overLappingElanAnnotations.map(e =>
      <seg type="elanLichteVernederlandsing" start={e.starttijd.toString} end={e.eindtijd.toString}>
        {e.tekst_lv}
      </seg>
        <seg type="elanZwareVernederlandsing" start={e.starttijd.toString} end={e.eindtijd.toString}>
          {e.tekst_zv}
        </seg>
    )}
      <seg type="alpinoLichteVernederlandsing">
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

  lazy val n_m = overLappingElanAnnotations.map(_.nAlpinos)
}
