package corpusprocessing.GCND

import corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.conll_u.AlpinoToken
import utils.Tokenizer

import scala.xml._

case class ElanAnnotation(elan_annotatie_id: Int,
                          transcriptie_id: Int,
                          annotatie_code: String,
                          opname_persoon_id: Int,
                          tekst_lv: String,
                          tekst_zv: String,
                          starttijd: Int,
                          eindtijd: Int
                         )
{
  type token = Tokenizer.Token

  lazy val overLappingAlpinoAnnotations: Seq[AlpinoAnnotation] = GCNDDatabase.alpinos.filter(e => // pas op ook op id filteren!
    e.starttijd >= starttijd & e.starttijd < eindtijd || e.eindtijd > starttijd & e.eindtijd <= eindtijd
  )

  lazy val allAlpinoTokens: Seq[(GCNDDatabase.Token, AlpinoToken)] = overLappingAlpinoAnnotations.flatMap(a => a.zipped)

  def pseudoFolia()  = {
    if (tekst_zv != null && tekst_lv != null) HeavyLightAlignment(this).align();

    lazy val speech_id = s"speech.elan.$elan_annotatie_id"
    lazy val alpinoStukje = overLappingAlpinoAnnotations.map(a => {

      <div class="alpinoAnnotation" begintime={Stuff.formatTime(a.starttijd)} endtime={Stuff.formatTime(a.eindtijd)}>

      <t class="alpinoLightDutchification">
        {a.text_lv}
      </t>
        <t class="alpinoHeavyDutchification">
          {a.text_zv}
        </t>
      </div>
    })

    <speech xml:id={speech_id}  begintime={Stuff.formatTime(starttijd)} endtime={Stuff.formatTime(eindtijd)}>
      <t class="elanLightDutchification">
        {tekst_lv}
      </t>
      <t class="elanHeavyDutchification">
        {tekst_zv}
      </t>
      {Comment("n_alpino_annotations: " +  overLappingAlpinoAnnotations.size.toString)}
      {alpinoStukje}
    </speech>
  }
  lazy val nAlpinos = overLappingAlpinoAnnotations.size
}

