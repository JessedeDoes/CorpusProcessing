package corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.conll_u.dependencyconversion

import corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.conll_u.CONLL

object conversionToSUD {
  val reference = CONLL.parseFile("/home/jesse/workspace/SUD_Dutch-LassySmall/nl_lassysmall-sud-train.conllu").map(s => s.sentencePlain.trim.replaceAll("\\s+", "")).toList
  def main(args: Array[String]) = {
    val location = args.headOption.getOrElse(Alpino2FlatDependencies.lassy.getOrElse("nope"))
    Alpino2DependencyConversion.transform(location, "/tmp/test.sud.txt", ConversionToSUDRules, reference)
  }
}
