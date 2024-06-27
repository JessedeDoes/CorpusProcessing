package corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.conll_u

case class UdToken(ID: String="_", FORM: String="_", LEMMA: String="_", UPOS: String="_", XPOS: String="_", FEATS: String="_", HEAD: String="_",
                   DEPREL: String="_", DEPS: String="_", MISC: String="_",
                   sent_id: String="_", language: String="und", subtokens:Seq[UdToken] = Seq()) {

  lazy val tokenId = s"$sent_id.$ID.$language"
  lazy val isMultiWordToken = !ID.matches("[0-9]+")
  lazy val subTokenIds: Seq[String] = if (isMultiWordToken) {
    val s1 = ID.split("-").toList;
    if (s1.forall(_.matches("^[0-9]+$"))) {
      val s2 = s1.map(_.toInt)
      Range.inclusive(s2.min, s2.max).map(_.toString).toSeq
    } else
    s1
  }
  else List()


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
