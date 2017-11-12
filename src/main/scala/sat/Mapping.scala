package sat
import Proposition._
object Mapping {

  val basicClauses = And(
    l("pos=INTJ") → l("cgn=TSW"),
    l("pos=NOUN") →  l("cgn=ZNW"),
    l("pos=PROPN") → (l("cgn=SPEC") ∧ l("cgn=deeleigen")),
    l("pos=VERB") ∧ l("mood=Part") → l("cgn=deelwoord"),
    l("mood=Part") →l("pos=VERB")
  )

  def mapToCGN(p:Proposition):Proposition =
  {
    val cgnTags:Set[String] = basicClauses.varsIn.filter(s => s.startsWith("cgn"))

    val translation = cgnTags.filter(t => {
        val lit = Literal(t)
        val extended = And(basicClauses, ¬(lit), p)
        !extended.isSatisfiable
      })
    val b = translation.map(Literal(_)).toList
    And(b: _*)
  }

  def main(args: Array[String]) =
  {
    val p  = l("mood=Part")
    val q = mapToCGN(p)
    println(s"$p => $q")
  }
}
