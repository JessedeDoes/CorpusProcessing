package sat
import org.sat4j.specs._
import org.sat4j.reader.DimacsReader
import org.sat4j.minisat.SolverFactory
import org.sat4j.specs.ISolver
//import scala.tools.
// https://github.com/Gbury/mSAT


import org.sat4j.minisat.SolverFactory
import org.sat4j.reader.DimacsReader
import org.sat4j.reader.ParseFormatException
import org.sat4j.reader.Reader
import org.sat4j.specs.ContradictionException
import org.sat4j.specs.IProblem
import org.sat4j.specs.ISolver
import java.io.FileNotFoundException
import java.io.IOException
import org.sat4j.core.VecInt

case class Dimacs(clauses: Seq[Seq[Int]])
{
  lazy val nbClauses:Int = clauses.size
  lazy val maxVar:Int = clauses.maxBy(c => c.max).max

  def solve() =
  {

    val solver = SolverFactory.newDefault

    solver.newVar(maxVar)
    solver.setExpectedNumberOfClauses(nbClauses);
    // Feed the solver using Dimacs format, using arrays of int
    // (best option to avoid dependencies on SAT4J IVecInt)
    (0 until  nbClauses).foreach(i => {
      val clause = clauses(i).toArray
      solver.addClause(new VecInt(clause))
    })
    if (solver.isSatisfiable)
      {
        println("Yup!")
        //solver.
      }
  }
}

object Proposition
{
  def ¬(p:Proposition) = Not(p)
  def p(s:String) = Literal(s)

  def example() =
  {

    val p:Proposition = And(
      Or(Literal("p"), (Literal("p"))),
      Or(Literal("q"), ¬(Literal("p")))
    )

    val d:Option[Dimacs] = p.toDimacs

    println(s"$p => $d")
    d.map(_.solve)
  }

  def example1(): Unit =
  {
    val p = Literal("p")
    val q = Literal("q")
    val r =  Literal("r")
    val s = Literal("s")
    val phi = (p ∨ q ∧ r) → ¬(s) // {\displaystyle \phi :=((p\lor q)\land r)\to (\neg s)}
    phi.tseytin
  }
  def main(args: Array[String]):Unit = example1
}

case class Literal(s: String) extends Proposition
case class Not(p:Proposition) extends Proposition
case class And(p: Proposition*) extends Proposition
case class Or(p: Proposition*) extends Proposition

trait Proposition
{
  import Proposition._

  def ∧(q:Proposition) = And(this,q)
  def ∨(q:Proposition) = Or(this,q)
  def →(q:Proposition) = ¬(this ∧ ¬(q))
  def ↔(q:Proposition) = (this→q) ∧ (q → this);

  def varsIn:Set[String] =
  {
    this match {
      case Literal(s) => Set(s)
      case Not(p) => p.varsIn
      case And(l @ _*) => l.flatMap(_.varsIn).toSet
      case Or(l @ _*) => l.flatMap(_.varsIn).toSet
    }
  }

  def isAtom():Boolean = this match {
    case Literal(_) => true
    case Not(p) => p.isAtom()
    case _ => false
  }

  def subFormulas:Set[Proposition] = this match {
    case And(l @ _*) => Set(this) ++ l.flatMap(_.subFormulas)
    case Or(l @ _*) => Set(this) ++ l.flatMap(_.subFormulas)
    case Not(p) => Set(this) ++ p.subFormulas
    case Literal(s) => Set(this)
  }

  def simpleConvert():Proposition =
  {
    this match {
      case And(l @ _*) =>
        {
          val clauses = l.map(_.simpleConvert)
          val flattened = clauses.flatMap(
            {
              case And(l @ _*) => l
              case  x:_ => Seq(x)
            })
          And(flattened: _*)
        }
      case Or(l @ _*) =>
      {
        val clauses = l.map(_.simpleConvert)
        val orz = clauses.flatMap(c => clauses.map(c1 => Or(c,c1)))
        And(orz: _*)
      }
      case Not(p) =>
        p match {
          case Not(q) => q.simpleConvert()
          case Literal(s) => p
          case And(p,q) => { val p0 = Or(Not(p), Not(q)); p0.simpleConvert() }
          case Or(p,q) =>

        }
    }
  }

  override def toString():String =
  this match {
    case Literal(s) => s
    case And(l @ _*) => l.map(_.toString).mkString(" ∧ ")
    case Or(l @ _*) => "(" + l.map(_.toString).mkString(" ∨ ") + ")"
    case Not(p) => "¬(" + p.toString + ")"
  }

  def tseytin =
  {
    val subMap:Map[Proposition, Proposition] =
      subFormulas.filter(p => !p.isInstanceOf[Literal]).zipWithIndex.toMap.mapValues(i => Literal(s"x_$i"))

    subMap.foreach(println)

    def flatten(p:Proposition):Proposition = p match {
      case And(l @ _*) => And( l.map(p => if (subMap.contains(p)) subMap(p) else flatten(p)) : _*)
      case Or(l @ _*) => And(l.map(p => if (subMap.contains(p)) subMap(p) else flatten(p)) : _*)
      case _ => p
    }

    val translatedClauses = subMap.keySet.map(p => subMap(p) ↔ flatten(p))
    translatedClauses.foreach(println)
  }

  def isSimpleDisjunction():Boolean =  this match
  {
    case Or(l @ _*) => l.forall(_.isAtom())
    case _ => false
  }

  def isCNF:Boolean = this match {
    case And(l @ _*) => l.forall(_.isSimpleDisjunction())
    case _ => false
  }

  def toDimacs:Option[Dimacs] =
  {
    if (!isCNF)
      return None
    val varMap:Map[String,Int] = varsIn.zipWithIndex.map({case (s,i:Int) => s -> (i+1)}).toMap
    var translation:Seq[Seq[Int]] = this match {
      case And(l @ _*) =>
        l.map(d => d match {
          case Or(l1 @ _*) => l1.map({
              case Literal(s) => varMap(s)
              case Not(Literal(s)) => -1 * varMap(s)
            }
          )
        })
    }
    Some(new Dimacs(translation))
  }
}



// case class Implies(p: Proposition, q:Proposition) extends Proposition




object Example {
  def main(args: Array[String]): Unit = {
    val solver = SolverFactory.newDefault
    solver.setTimeout(3600) // 1 hour timeout

    val reader = new DimacsReader(solver)
    // CNF filename is given on the command line
    try {
      val problem = reader.parseInstance(args(0))
      if (problem.isSatisfiable) {
        System.out.println("Satisfiable !")
        System.out.println(reader.decode(problem.model))
      }
      else System.out.println("Unsatisfiable !")
    } catch {
      case e: FileNotFoundException =>

      // TODO Auto-generated catch block
      case e: ParseFormatException =>

      case e: IOException =>

      case e: ContradictionException =>
        System.out.println("Unsatisfiable (trivial)!")
      case e: TimeoutException =>
        System.out.println("Timeout, sorry!")
    }
  }
}