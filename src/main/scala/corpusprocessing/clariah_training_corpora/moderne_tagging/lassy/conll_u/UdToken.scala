package corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.conll_u

case class UdToken(ID: String="_", FORM: String="_", LEMMA: String="_", UPOS: String="_", XPOS: String="_", FEATS: String="_", HEAD: String="_",
                   DEPREL: String="_", DEPS: String="_", MISC: String="_",
                   sent_id: String="_", language: String="und", subtokens:Seq[UdToken] = Seq()) {

  lazy val tokenId = s"$sent_id.$ID.$language"

  def toXML(sent_id: String, language: String) = {
    val feats = if (FEATS != "_") "|" + FEATS else ""
    <w xml:id={tokenId} pos={UPOS} msd={s"UPosTag=$UPOS$feats"} lemma={LEMMA} ana={s"xpos=$XPOS|misc=$MISC"}>
      {FORM}
    </w>
  }

  def toCONLL(): String = {
    List(ID,FORM,LEMMA,UPOS,XPOS,FEATS,HEAD,DEPREL,DEPS,MISC).map(_.replaceAll("\\s+", " ").trim.replaceAll("^$","_")).mkString("\t")
  }

  lazy val position = ID.toInt

  def precedes(t: UdToken) = this.position < t.position
}
