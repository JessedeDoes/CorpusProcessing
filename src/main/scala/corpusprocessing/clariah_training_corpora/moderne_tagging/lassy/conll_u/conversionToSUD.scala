package corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.conll_u

object conversionToSUD {
  val reference = CONLL.parseFile("/home/jesse/workspace/SUD_Dutch-LassySmall/nl_lassysmall-sud-train.conllu").map(s => s.sentencePlain.trim.replaceAll("\\s+", "")).toList
  def main(args: Array[String]) = {

    val location = args.headOption.getOrElse(LassyXML2FlatDependencies.lassy.getOrElse("nope"))
    LassyXML2FlatDependencies.transform(location, "/tmp/test.sud.txt", ConversionToSUDRules, reference)
  }
}
