package sparql2xquery
import Mappings._

case class TripleMapping(pm: PredicateMapping)
{
  def apply(sName: String, oName: String, predicate: String): SimpleSelect =
  {
    val base = pm(predicate)
    val m = Map("subject" -> sName, "object" -> oName)
    val mapped = base.renameVars(m)
    SimpleSelect(mapped.pathExpressions, mapped.variables, Set(sName, oName)).anonymizeVars()
  }
}
