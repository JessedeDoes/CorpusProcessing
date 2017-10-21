package datalog

import it.unibz.inf.ontop.model.CQIE
import scala.collection.JavaConversions._

case class Rule(r: CQIE) extends Datalog
{
  val head = r.getHead
  val functions = r.getBody.map(r => function(r)).toList
  override def toString():String = ("head:"  + head,functions.toString()).toString()
}
