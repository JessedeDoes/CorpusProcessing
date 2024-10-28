package corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.conll_u

object conversionToSUD {
  def main(args: Array[String]) = {
    val location = args.headOption.getOrElse(LassyXML2FlatDependencies.lassy.getOrElse("nope"))
    LassyXML2FlatDependencies.transform(location, "/tmp/test.sud.txt", ConversionToSUDRules)
  }
}
