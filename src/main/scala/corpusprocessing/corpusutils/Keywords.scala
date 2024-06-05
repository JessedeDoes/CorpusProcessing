package corpusprocessing.corpusutils

case class Keywords(lemmaList: Option[Set[String]]) {

}


/*
after   na      Adverb  1       105     naderen
after   na      Adverb  1       105     nae
after   na      Adverb  1       105     naeder
after   na      Adverb  1       105     naer
after   na      Adverb  1       105     naerder
 */

//case class Lemma(en: String, nl: String, id: String, )
object Keywords {
  val keywordList = "/mnt/Projecten/Diamant/Blankenborg/en_nl.sorted.tsv"
  val variantList = "/mnt/Projecten/Diamant/Blankenborg/variants.tsv"
}


