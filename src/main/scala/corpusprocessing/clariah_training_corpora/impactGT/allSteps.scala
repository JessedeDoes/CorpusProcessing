package corpusprocessing.clariah_training_corpora.impactGT

object allSteps {
  val xslDir = "/mnt/Projecten/Corpora/Historische_Corpora/ImpactGT/XSL/"
  val sheet = "p2t_simple.xsl"
  def main(args: Array[String])   = {
    //enhanceMetadata.main(Array())  // get metadata from KB whenever possible
    //makegroupedLists.main(Array()) // group page files by metadata in order to get one TEI doc
    utils.XSLT.main( // run the XSLT stylesheet to convert to TEI
      Array(
        xslDir + sheet,
        "/mnt/Projecten/Corpora/Historische_Corpora/ImpactGT/Page/DDDEnhanced/DDDGroupedLists/",
        "/mnt/Projecten/Corpora/Historische_Corpora/ImpactGT/Page/DDDEnhanced/DDDTEI/"))
  }
}
