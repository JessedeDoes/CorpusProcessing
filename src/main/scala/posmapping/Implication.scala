package posmapping

object Implication {
  implicit def decl(pos: String, feature: String, value: String, mandatoryFeatures: Set[String]):  Implication = {
    Implication(t => {
      if (t.pos == pos && t.features.exists(x => x.name == feature && x.value == value))
        mandatoryFeatures.forall(z => t.features.exists(_.name == z))
      else true
    }, s"$pos $feature $value => $mandatoryFeatures")
  }

  implicit def impl(pos: String, feature: String, value: String, mandatoryFeatures: Set[(String,String)]):  Implication = {
    Implication(t => {
      if (t.pos == pos && t.features.exists(x => x.name == feature && x.value == value))
        mandatoryFeatures.forall(z => t.features.exists(f => f.name == z._1 && f.value == z._2))
      else true
    }, s"$pos $feature $value => $mandatoryFeatures")
  }
}

case class Implication(f: Tag => Boolean, description: String) {

}
