package bab_aanvulling

object translateMetadataValues {

  val ageMap = Map("0-12" -> "&lt;30", "12-20" -> "&lt;30", "20-30" -> "&lt;30",
    "30-40" -> "30-50", "40-50" -> "30-50",
    "50-60" -> "&gt;50", "60+" -> "&gt;50");

  val genderMap = Map("man" -> "male", "vrouw" -> "female", "onbekend" -> "unknown");

  val typeMap = Map("prive" -> "private", "zakelijk" -> "business", "combinatie" -> "combination");

  val autoMap = Map("autograaf" -> "autograph", "niet-autograaf" -> "non-autograph", "onduidelijk" -> "uncertain", "NULL" -> "uncertain", "combinatie" -> "combination")

  val classMap = Map("laag" -> "low", "hoog" -> "high", "midden-hoog" -> "middle-high", "midden-laag" -> "middle-low", "onbekend" -> "unknown", "NULL" -> "unknown")

  val relMap =
    Map(
      "(ex)echtgenoot" -> "(ex-)husband",
      "(ex)echtgenote" -> "(ex-)wife",
      "anders" -> "other",
      "broer / zwager" -> "brother / brother in law",
      "dochter" -> "daughter",
      "geliefde (m) / minnaar" -> "beloved (m)",
      "geliefde (v) / minnares" -> "beloved (f)",
      "kennis" -> "acquaintance",
      "kleinzoon" -> "grandson",
      "meerdere familieleden" -> "family members",
      "moeder" -> "mother",
      "neef" -> "nephew/cousin",
      "nicht" -> "niece/cousin",
      "grootmoeder" -> "grandmother",
      "oom" -> "uncle",
      "tante" -> "aunt",
      "vader" -> "father",
      "vriend" -> "friend (m)",
      "vriendin" -> "friend (f)",
      "werkgever" -> "employer",
      "werknemer" -> "employee",
      "zakenrelatie" -> "business relation",
      "zoon" -> "son",
      "zus / zuster / schoonzus" -> "sister / sister in law",
      "NULL" -> "unknown"
    );

  val mapMap = Map("afz_geb_lftcat" -> ageMap,
    "type_brief" -> typeMap,
    "autograaf" -> autoMap,
    "afz_geslacht" -> genderMap,
    "afz_klasse" -> classMap,
    "relatie_a" -> relMap,
    "relatie_b" -> relMap,
    "afz_rel_tot_adr" -> relMap)

  def replaceValue(name: String, value: String) = {

    val baseValue = if (name == "regiocode" && value == "Noord-Holland") "Noord-Holland (excluding Amsterdam)" else {
      if (mapMap.contains(name)) {
        if (mapMap(name).contains(value)) mapMap(name)(value) else {
          //if (!(Set("","null").contains(value)))
            //Console.err.println(s"Unknown value for $name: $value")
          "unknown"
        }
      } else value;
    }

    if (baseValue.toLowerCase.contains("onbekend") || baseValue.toLowerCase.contains("null") ||
      (Set("adr_naam_norm", "afz_naam_norm").contains(name) && baseValue.trim.isEmpty))
      "unknown" else baseValue;
  }

}
