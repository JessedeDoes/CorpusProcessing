package db2rdf

import database.Configuration

object Settings {
  val gigantHilexDBOld = new database.Database(Configuration("x", "svowdb06","gigant_hilex_candidate", "fannee", "Cric0topus"))
  val gigantHilexDB = new database.Database(Configuration("x", "svowdb16.ivdnt.loc","gigant_hilex_candidate", "dba", "vercingetorix"))
  val doLexCit = false
  val useLangStrings = false
  val outputDefinitionsAndQuotations = true
  val miniDiamant = false
  val outputFolderForDiamantRDF =  if (miniDiamant) "/data/Diamant/RDF_kat" else "/data/Diamant/RDF_modlem" // "/mnt/Projecten/CLARIAH/Scratch"

  val data_schema = if (miniDiamant) "mini_hilex" else "data"
  val sense_schema = if (miniDiamant) "mini_hilex" else "diamant"
}
