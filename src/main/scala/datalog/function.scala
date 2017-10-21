package datalog

import it.unibz.inf.ontop.model.{Function, Term, ValueConstant, Variable}
import scala.collection.JavaConversions._

case class function(f: Function) extends Datalog
{
  val terms = f.getTerms.map(t => term(t)).toList
  val symbol = f.getFunctionSymbol

  def term(t: Term) =
  {
    t match {
      case f1: Function => function(f1)
      case v: Variable => variable(v)
      case x: ValueConstant => valueConstant(x)
      case _ => "unknown:" + t.getClass.getName
    }
  }
  override def toString():String = s"$symbol(\n${terms.toString()}\n)"
}
