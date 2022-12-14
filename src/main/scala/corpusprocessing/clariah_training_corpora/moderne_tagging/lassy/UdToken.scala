package corpusprocessing.clariah_training_corpora.moderne_tagging.lassy

case class UdToken(ID: String, FORM: String, LEMMA: String, UPOS: String, XPOS: String, FEATS: String, HEAD: String, DEPREL: String, DEPS: String, MISC: String, sent_id: String, language: String) {

  lazy val tokenId = s"$sent_id.$ID.$language"


  def toXML(sent_id: String, language: String) = {
    val feats = if (FEATS != "_") "|" + FEATS else ""
    <w xml:id={tokenId} pos={UPOS} msd={s"UPosTag=$UPOS$feats"} lemma={LEMMA} ana={s"xpos=$XPOS|misc=$MISC"}>
      {FORM}
    </w>
  }

  lazy val position = ID.toInt

  def precedes(t: UdToken) = this.position < t.position
}
