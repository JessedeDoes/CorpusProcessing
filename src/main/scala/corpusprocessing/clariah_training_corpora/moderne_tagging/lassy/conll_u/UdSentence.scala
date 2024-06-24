package corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.conll_u

import corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.{Sentence, cgn_tdn}

case class UdSentence(sent_id: String, language: String, tokens: Seq[UdToken], lines: Seq[String] = Seq()) {

  def hasContent(): Boolean = tokens.exists(_.FORM != "_")
  def isValid(): Boolean = {
    val roots = tokens.filter(t => t.DEPREL == "root")
    val nRoots = roots.size

    lazy val check0 = {
      val ids: Seq[Int] = tokens.map(_.ID.toInt)
      val check: Seq[Int] = (1 to tokens.size).toSeq
      ids == check
    }

    if (false && !check0)
      false
    else
    if (!(nRoots == 1))
    {
      // println(s"!!!!!!!!!!!!!!! nRoots != 1: $nRoots (${roots.map(_.ID)})")
      // println(this.toCONLL())
      false
    } else
    {
      val rootTo0 = tokens.exists(t => t.DEPREL == "root" && t.HEAD == "0")

      val root = tokens.find(_.DEPREL == "root").head
      val rootCoversAll = descendant(root) ++ Set(root) == tokens.toSet

      if (!rootCoversAll) {
        // println("!!!!!!!!!!!!!!! Unreachable nodes!")
        // println(this.toCONLL())
      } else {
        // println(s"Toppie: ${descendant(root).size}, ${tokens.size}")
      }
      val noCycle = !cycle(root)
      nRoots == 1 && rootCoversAll && !cycle(root)
    }
  }

  def children(t: UdToken) = tokens.filter(_.HEAD == t.ID)

  def dependencyPattern(t: UdToken) = children(t).filter(_.DEPREL != "punct").map(_.DEPREL).sorted.mkString("|") //

  def descendant(t: UdToken, collected: Set[UdToken] = Set()): Set[UdToken] = {
    val c = children(t).filter(x => !collected.contains(x)).toSet.filter(_ != t)
    c ++ c.flatMap(x => descendant(x, collected ++ Set(t)))
  }

  def cycle(t: UdToken, collected: Set[UdToken] = Set()): Boolean= {
    if (collected.contains(t))  {
       Console.err.println(s"!!!!!!!!!!!!!!!Cycle: $t in ${collected.map(_.ID)}")
       println(this.toCONLL())
       true
    }
    else {
      val c1 =  collected ++ Set(t)
      children(t).exists(c => cycle(c, c1))
    }
  }
  lazy val plainText = tokens.zipWithIndex.map({case (t,i) => t.FORM + (if (t.MISC.contains("SpaceAfter=No")) "" else " ") }).mkString("").trim

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

  val deprel_column = 7

  def patchLines() = {
    val newLines = lines.map(l => {
      val fields = l.split("\t")
      if (fields.size > deprel_column) {
        val id = fields(0)
        val oldRel = fields(deprel_column)
        val newRel = tokens.find(t => t.ID == id).map(_.DEPREL).getOrElse("rel_not_found_for " + id + "<" + oldRel + ":" +  tokens.size)
        val newRelHead = fields(deprel_column - 1) + ":" + newRel
        val newFields = fields.patch(deprel_column, Seq(newRel, newRelHead), 2)
        newFields.mkString("\t")
      } else {
        l
      }
    })
    this.copy(lines = newLines)
  }

  def rebaseIds() = {
    if (this.tokens.isEmpty) this else {
      val id0 = tokens.head.ID.replaceAll("-.*", "").toInt
      this.copy(tokens = tokens.map(t => {
        val newId = t.ID.split("-").map(x => x.toInt - id0 + 1).mkString("-")
        t.copy(ID = newId, UPOS=TEI2CONLLU.getPos(t.XPOS), XPOS="_")
      }))
    }
  }

  def toCONLL(rebase: Boolean = true) = {
    (Seq("", s"# sent_id = ${this.sent_id}",  s"# text = ${this.plainText}" ) ++
      (if (rebase) rebaseIds() else this).tokens.map(_.toCONLL()))
      .mkString("\n") ++ "\n"
  }


  def asTEI() = {
    val words = tokens.map(t => {
      val msd = s"UPosTag=${t.UPOS}" + (if (t.FEATS.contains("|")) "|"  + t.FEATS else "");
      <w xml:id={this.sent_id + "." + t.ID} subtype={dependencyPattern(t)} lemma={t.LEMMA} pos={t.UPOS} msd={s"UPosTag=${t.UPOS}|${t.FEATS}"}>{t.FORM}</w>
    }) // PC
    val links = tokens.map(t =>  {
        val headPart = if (t.DEPREL == "root") "" else s".${t.HEAD}"
        <link ana={"ud-syn:" + t.DEPREL} target={s"#${this.sent_id}$headPart #${this.sent_id}.${t.ID}"}/> })
    val linkGrp = <linkGrp targFunc="head argument" type="UD-SYN">{links}</linkGrp>;
    <s xml:lang={language} xml:id={this.sent_id}>{words}{linkGrp}</s>
  }
}
