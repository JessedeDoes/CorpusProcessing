package corpusprocessing.GCND

case class ElanAnnotation(elan_annotatie_id: Int,
                          transcriptie_id: Int,
                          annotatie_code: String,
                          opname_persoon_id: Int,
                          tekst_lv: String,
                          tekst_zv: String,
                          starttijd: Int,
                          eindtijd: Int
                         ) {
  lazy val overLappingElanAnnotations = GCNDDatabase.alpinos.filter(e =>
    e.starttijd >= starttijd & e.starttijd <= eindtijd || e.eindtijd >= starttijd & e.eindtijd <= eindtijd
  )

  lazy val nAlpinos = overLappingElanAnnotations.size
}

