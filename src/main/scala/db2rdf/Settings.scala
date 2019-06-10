package db2rdf

import database.Configuration

object Settings {
  val gigantHilexDB = new database.Database(Configuration("x", "svprre02","gigant_hilex_candidate_update", "postgres", "inl"))
  val doLexCit = false
  val useLangStrings = false
}
