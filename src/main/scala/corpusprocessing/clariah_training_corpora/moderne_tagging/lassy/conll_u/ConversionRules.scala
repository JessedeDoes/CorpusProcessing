package corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.conll_u

trait ConversionRules {
  def betterRel(n: AlpinoNode): String

  def findConstituentHead(n: AlpinoNode, allowLeaf: Boolean = false): Option[AlpinoNode]
}
