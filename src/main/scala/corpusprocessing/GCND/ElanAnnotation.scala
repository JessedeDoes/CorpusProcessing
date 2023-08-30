package corpusprocessing.GCND

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
  lazy val overLappingAlpinoAnnotations: Seq[AlpinoAnnotation] = GCNDDatabase.alpinos.filter(e => // pas op ook op id filteren!
    e.starttijd >= starttijd & e.starttijd <= eindtijd || e.eindtijd >= starttijd & e.eindtijd <= eindtijd
  )

  def pseudoFolia()  = {
    lazy val speech_id = s"speech.elan.$elan_annotatie_id"
    lazy val alpinoStukje = overLappingAlpinoAnnotations.map(a => {
      <div class="alpinoAnnotation">
      <t class="alpinoLightDutchification">
        {a.text_lv}
      </t>
        <t class="alpinoHeavyDutchification">
          {a.text_zv}
        </t>
      </div>
    })

    <speech xml:id={speech_id}>
      <t class="elanLightDutchification">
        {tekst_lv}
      </t>
      <t class="elanHeavyDutchification">
        {tekst_zv}
      </t>
      {alpinoStukje}
    </speech>
  }
  lazy val nAlpinos = overLappingAlpinoAnnotations.size
}

