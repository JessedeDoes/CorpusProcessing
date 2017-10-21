package datalog

import it.unibz.inf.ontop.model.DatalogProgram
import scala.collection.JavaConversions._

case class Program(d: DatalogProgram) extends Datalog
{
  val rules = d.getRules.map(r => Rule(r)).toList
  override def toString():String = rules.toString()
}
