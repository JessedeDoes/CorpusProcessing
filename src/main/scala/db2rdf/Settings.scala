package db2rdf

import database.Configuration

object Settings {
  val gigantHilexDB = new database.Database(Configuration("x", "svowdb06","gigant_hilex_candidate", "fannee", "Cric0topus"))
  val doLexCit = false
  val useLangStrings = false
  val outputDefinitionsAndQuotations = false
  val outputFolderForDiamantRDF = "/mnt/Projecten/CLARIAH/Scratch"
}
