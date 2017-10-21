package datalog

import it.unibz.inf.ontop.model.Variable

case class variable(v: Variable) extends Datalog
{
  val name = v.getName
}
