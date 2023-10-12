package zeeuws.xml2db

object Regions {
  val r0 = Map("W." -> "Walcheren",
    "de Westhoek van Sch." -> "Weetikveel",
    "West-Fl." -> "West-Flakkee",
    "grensstreek" -> "grensstreek",
    "Z. dl." -> "zuidelijk deel van WZO",
    "Z.B." -> "Zuid-Beveland",
    "Z.dl" -> "zuidelijk deel van WZO",
    "z.dl. van Z.V.O." -> "zuidelijk deel van WZO",
    "Z.eil" -> "Zeeuwse eilanden",
    "Z.eil." -> "Zeeuwse eilanden",
    "z.eil.beh.w." -> "Zeeuwse eilanden behalve Walcheren",
    "Z.V." -> "Zeeuws Vlaanderen (niet zeker of dit voorkomt)",
    "Z.V.O" -> "Oost-Zeeuws-Vlaanderen",
    "Z.V.O." -> "Oost-Zeeuws-Vlaanderen",
    "Z.V.W en O." -> "Oost en West-Zeeuws-Vlaanderen",
    "Z.V.W." -> "West-Zeeuws-Vlaanderen",
    "Zvo-zd" -> "zuidelijk deel van WZO",
    "Alg" -> "full_tdb",
    "Alg." -> "full_tdb",
    "Alg." -> ",1",
    "D." -> "full_tdb",
    "Duiveland" -> "full_tdb",
    "G." -> "full_tdb",
    "Goeree" -> "full_tdb",
    "L. v.Ax." -> "full_tdb",
    "L.v. H." -> "full_tdb",
    "L.v.Ax." -> "full_tdb",
    "L.v.H." -> "full_tdb",
    "N.B." -> "full_tdb",
    "Ofl." -> "full_tdb",
    "Oost-Fl." -> "full_tdb",
    "Oost-T." -> "full_tdb",
    "Oost-Tolen" -> "full_tdb",
    "Oost-Z.B." -> "full_tdb",
    "Oost.Z.B." -> "full_tdb",
    "Sch-D" -> "full_tdb",
    "Sch." -> "full_tdb",
    "Sch.-D" -> "full_tdb",
    "Sch.-D." -> "full_tdb",
    "T." -> "full_tdb",
    "West-Tolen" -> "full_tdb",
    "West.-Fl." -> "full_tdb",
    "West.T." -> "full_tdb",
    "West.Z.B." -> "full_tdb",
    "Z.V.W. en O." -> "full_tdb",
    "alg" -> "full_tdb",
    "alg." -> "full_tdb",
    "alg. Sch-D" -> "full_tdb",
    "grensstreek Z.V.W." -> "full_tdb",
    "west-Sch." -> "full_tdb"
  ).map({ case (k, v) => k.toLowerCase().trim -> v })

  val r2 = r0 ++ r0.map({ case (k, v) => k.replaceAll(" ", "") -> v })

  def apply(p: String): String = {
    val p1 = p.trim.toLowerCase().replaceAll(" ", "")
    r2.getOrElse(p1, "")
  }

  def contains(p: String): Boolean = {
    val p1 = p.trim.toLowerCase().replaceAll(" ", "")
    // if (p1.contains("eil")) println("Check: " + p1  + " " + r2.contains(p1))
    r2.contains(p1)
  }
  // println("keys for region map" + r2.keySet)
}
