package corpusprocessing.clariah_training_corpora.moderne_tagging.lassy

object cgn_tdn {
  lazy val cgn_tdn_mapping = io.Source.fromFile("data/TDN/Corpora/CGN/tag_mapping_cgn_with_core.txt").getLines().map(l => l.split("\\t").toList).map(l => {
    val cgn = l(1)
    val tdn = l(2)
    val tdn_core = l(3)
    cgn -> (tdn, tdn_core)
  }).toMap

  def xpos2cgn(t: String) = {
    val t0 = t.replaceAll("\\|", ",").replaceAll("^([A-Z]+),(.*)", "$1($2)")
    val t1 = if (t0.endsWith(")")) t0 else s"$t0()"
    //System.err.println(s"$t -> $t1")
    t1
  }

  def cgn2tdncore(t: String) = cgn_tdn_mapping.get(t).map(_._2).getOrElse("UNK_" + t)

  def xpos2tdncore(t: String) = cgn_tdn_mapping.get(xpos2cgn(t)).map(_._2).getOrElse("UNK_" + t)

}
