package db2rdf

import database.Configuration

object Settings {
  val gigantHilexDB = new database.Database(Configuration("x", "svowdb06","gigant_hilex_candidate", "fannee", "Cric0topus"))
  val doLexCit = false
  val useLangStrings = false
  val outputDefinitionsAndQuotations = true
  val miniDiamant = true
  val outputFolderForDiamantRDF =  if (miniDiamant) "/data/Diamant/RDF_kat" else "/data/Diamant/RDF" // "/mnt/Projecten/CLARIAH/Scratch"

  val data_schema = if (miniDiamant) "mini_hilex" else "data"
  val sense_schema = if (miniDiamant) "mini_hilex" else "diamant"
}
