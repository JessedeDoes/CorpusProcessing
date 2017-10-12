import it.unibz.inf.ontop.model._
import scala.collection.JavaConversions._

trait Datalog

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

case class valueConstant(v: ValueConstant) extends Datalog

case class variable(v: Variable) extends Datalog
{
  val name = v.getName
}

case class Rule(r: CQIE) extends Datalog
{
  val head = r.getHead
  val functions = r.getBody.map(r => function(r)).toList
  override def toString():String = ("head:"  + head,functions.toString()).toString()
}

case class Program(d: DatalogProgram) extends Datalog
{
  val rules = d.getRules.map(r => Rule(r)).toList
  override def toString():String = rules.toString()
}
