package corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.conll_u

import corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.{Sentence, cgn_tdn}

case class UdSentence(sent_id: String, language: String, tokens: Seq[UdToken]) {

  lazy val sentencePlain = tokens.map(_.FORM).mkString(" ")
  def toXML() = <s xml:lang={language} n={sent_id} xml:id={s"$sent_id.$language"}>
    {tokens.map(_.toXML(sent_id, language))}{linkGrp}
  </s>

  lazy val n2id = tokens.map(t => t.ID -> t.tokenId).toMap

  lazy val links: Seq[(String, String, String)] = tokens.map(t => {
    val id = n2id(t.ID)
    (t.DEPREL, id, n2id.getOrElse(t.HEAD, id))
  })

  def head(t: UdToken) = tokens.find(_.ID == t.HEAD)


  def xpos_enhanced(t: UdToken) = (t.UPOS, t.DEPREL, head(t)) match {

    // scheidbare adverbia en werkwoorden

    case ("ADP", "compound:prt", h) => h.map(h => h.XPOS + "_deel_sep_ww" + (if (t.precedes(h)) "_b" else "_f")).getOrElse(t.XPOS) // enzovoorts ...

    case (_, "case", h) if h.exists(t => t.LEMMA.toLowerCase == "er") => h.map(h0 => h0.XPOS + "_deel_sep_adv" + (if (t.precedes(h0)) "_b" else "_f")).getOrElse(t.XPOS)


    case ("VERB", _, _) => {
      val particle = tokens.find(t1 => t1.HEAD == t.ID && t1.DEPREL == "compound:prt")

      if (particle.nonEmpty) {
        t.XPOS + "_head_sep_vrb" + (if (t.precedes(particle.get)) "_b" else "_f")


      } else {
        val copula = tokens.find(t1 => t1.HEAD == t.ID && t1.DEPREL == "cop")
        if (copula.nonEmpty && t.XPOS.matches(".*(vd|od).*")) {
          println(copula.get.FORM -> t.FORM)
          "ADJ|vrij|basis|zonder"
        } else t.XPOS
      }
    }

    case (_, _, _) if (t.LEMMA == "er") => {
      val particle = tokens.find(t1 => t1.HEAD == t.ID && t1.DEPREL == "case")
      //if (particle.nonEmpty) println(particle -> t)
      if (particle.nonEmpty)
        t.XPOS + "_head_sep_adv" + (if (t.precedes(particle.get)) "_b" else "_f")
      else t.XPOS
    }

    // transcategorisaties (ook al uit position te halen)

    /*
    case ("ADJ", "obj", h) => t.XPOS + "_transcat_to_n_obj"
    case ("ADJ", "nsubj", h) => t.XPOS + "_transcat_to_n_nsubj"
    case ("ADJ", "iobj", h) => t.XPOS + "_transcat_to_n_iobj"
    */

    case _ => t.XPOS
  }

  def xpos_converted(t: UdToken): String = {
    val parts: Array[String] = xpos_enhanced(t).split("_")

    val p0: String = cgn_tdn.xpos2tdncore(parts(0))

    val p1 = if (p0.matches(".*d-p.*w-p.*") && t.LEMMA.toLowerCase.matches("d.*|hetgeen")) p0.replaceAll("d-p.*w-p", "d-p") else p0
    val p2 = if (p0.contains("PC")) "LET" else p1
    val p3 = if (p0.matches("ADV.*uncl.*"))  {
      p2.replaceAll("uncl","reg") // add check for pronominal adverbs? or do that in the dependency check?
    } else p2
    (if (parts.size > 1) p3 + "_" + parts.drop(1).mkString("_") else p3)
      .replaceAll("_.*_", "_") // keep only b,f fttb
  }

  lazy val linkGrp = <linkGrp>
    {links.map(t => <link ana={"ud-syn:" + t._1} target={s"#${t._3} #${t._2}"}/>)}
  </linkGrp>

  lazy val sent: Sentence = Sentence("", tokens.map(_.FORM).toList, tokens.map(xpos_converted(_)).toList, tokens.map(_.LEMMA).toList) // todo add lemmata

  lazy val TEI = <s xml:id={sent_id}>
    {tokens.map(t => <w pos={xpos_converted(t)} lemma={t.LEMMA}>{t.FORM}</w>)}
  </s>

  lazy val conll = {
    s"# $sent_id\n" +
    tokens.map(t => s"${List(t.ID,t.FORM,t.DEPREL).mkString("\t")}").mkString("\n")
  }
  // /mnt/Projecten/Corpora/TrainingDataForTools/LassyKlein/LassySmall/Treebank/dpc-bal-001236-nl-sen/dpc-bal-001236-nl-sen.p.10.s.3
  lazy val filename = sent_id.replaceAll(".*/","").replaceAll("\\.p\\..*","")
  lazy val paragraph  = sent_id.replaceAll(".*/","").replaceAll("\\.s\\..*","")
}
