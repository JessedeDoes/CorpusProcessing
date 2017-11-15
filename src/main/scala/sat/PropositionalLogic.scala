package sat
import sat.Proposition.¬
//import scala.tools.
// https://github.com/Gbury/mSAT


import org.sat4j.core.VecInt
import org.sat4j.minisat.SolverFactory

case class Literal(s: String) extends Proposition
case class Not(p:Proposition) extends Proposition
case class And(p: Proposition*) extends Proposition
case class Or(p: Proposition*) extends Proposition
case class Implies(p: Proposition, q:Proposition) extends Proposition
case class Equiv(p: Proposition, q:Proposition) extends Proposition

object Proposition
{
  def ¬(p:Proposition) = Not(p)
  def l(s:String) = Literal(s)
  def p(s:String) = Literal(s)
}

trait Proposition
{
  import Proposition._

  def ∧(q:Proposition) = And(this,q)
  def ∨(q:Proposition) = Or(this,q)
  def →(q:Proposition):Proposition = ¬(this ∧ ¬(q))
  def ↔(q:Proposition):Proposition = (this→q) ∧ (q → this)

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

  def simpleCNFConversion():Proposition =
  {
    val noDouble = this.removeDoubleNegation()
    this.removeDoubleNegation() match {
      case Literal(s) => And(noDouble)

      case And(l @ _*) =>
      {
        val clauses = l.map(_.simpleCNFConversion)
        //Console.err.println(s"and: ($noDouble): " + clauses)
        val flattened = clauses.flatMap(
          {
            case And(l @ _*) => l
            case  x:Any => Seq(x)
          })
        //Console.err.println("flattened:" + flattened)
        And(flattened: _*)
      }

      case Or(p, q) =>
      {
        val p1 = p.simpleCNFConversion()
        val q1 = q.simpleCNFConversion()
        (p1,q1) match {
          case (And(l1 @ _*), And(l2 @ _*)) =>
            val orz = l1.flatMap(l => l2.map(k => Or(k,l)))
            // Console.err.println(orz.mkString(" ## "))
            And(orz: _*)
        }
      }

      case Or(p) => p.simpleCNFConversion()

      case Or(l @ _*) => {
        val h = l.head
        val t = Or(l.tail : _*)
        val x = Or(l.head, t)
        t.simpleCNFConversion()
      }

      case Not(p) =>
        p match {
          case Not(q) => q.simpleCNFConversion()
          case Literal(s) => And(Not(p))
          case And(p1,q1) => { val p0 = Or(Not(p1), Not(q1)); p0.simpleCNFConversion() }
          case Or(p1,q1) =>  { val p0 = And(Not(p1), Not(q1)); p0.simpleCNFConversion() }
        }
    }
  }

  override def toString():String =
    this match {
      case Literal(s) => s
      case And(l @ _*) => "(" + l.map(_.toString).mkString(" ∧ ") + ")"
      case Or(l @ _*) => "(" + l.map(_.toString).mkString(" ∨ ") + ")"
      case Not(p) => "¬(" + p.toString + ")"
      case Implies(p,q) => p.toString() +  "→" + q.toString()
      case Equiv(p,q) => p.toString() +  "↔" + q.toString()
    }

  def removeDoubleNegation():Proposition = this match {
    case Literal(s) => this
    case And(l @ _*) =>  And(l.map(_.removeDoubleNegation()): _*)
    case Or(p:Proposition) => p.removeDoubleNegation()
    case Or(l @ _*) =>  Or(l.map(_.removeDoubleNegation()): _*)
    case Not(Not(p)) => p
    case _ => this
  }

  def flattenOrs():Proposition = this match {
    case Literal(s) => this
    case And(l @ _*) =>  And(l.map(_.flattenOrs()): _*)
    case Or(l @ _*) => val clauses = l.map(_.flattenOrs)
      val flattened = clauses.flatMap(
        {
          case Or(l @ _*) => l
          case  x:Any => Seq(x)
        })
      // Console.err.println("flattened:" + flattened)
      Or(flattened: _*)
    case Not(Not(p)) => p
    case _ => this
  }

  // TODO niet af
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
    case Or(l @ _*) => l.forall(x => x.isAtom() || x.isSimpleDisjunction())
    case _ => this.isAtom()
  }

  def isCNF:Boolean = this match {
    case And(l @ _*) => l.forall(_.isSimpleDisjunction())
    case _ => false
  }

  def translate(varMap:Map[String,Int]): Seq[Seq[Int]] =
  {
    val cnfje = if (isCNF) this else  this.simpleCNFConversion().flattenOrs()
    cnfje match {
      case And(l @ _*) =>
        l.map(d => d match {
          case Or(l1 @ _*) => l1.map({
            case Literal(s) => varMap(s)
            case Not(Literal(s)) => -1 * varMap(s)
          }
          )
          case Literal(s) => Seq(varMap(s))
          case Not(Literal(s)) => Seq(-1 * varMap(s))
        })
    }
  }

  lazy val dimacs:Option[Dimacs] = toDimacs

  def toDimacs:Option[Dimacs] =
  {
    val varMap:Map[String,Int] = varsIn.zipWithIndex.map({case (s,i:Int) => s -> (i+1)}).toMap
    var translation:Seq[Seq[Int]] = translate(varMap)
    Some(new Dimacs(translation, varMap))
  }

  def addToDimacs(dima: Dimacs): Option[Dimacs] =
  {
    var max:Int = dima.varMap.values.max
    val varMapExtra:Map[String,Int] = varsIn.diff(dima.varMap.keySet).zipWithIndex.map({ case (s,i:Int) => s -> (max + i+1)} ).toMap
    val varMap = dima.varMap ++ varMapExtra
    var translation = translate(varMap)
    Some(new Dimacs(dima.clauses ++ translation, varMap))
  }

  def isConsistentWith(p: Proposition): Boolean =
  {
    val dima = p.addToDimacs(this.dimacs.get).get
    dima.isSatisfiable()
  }

  def isSatisfiable(): Boolean =
  {
    val cnf = this.simpleCNFConversion().flattenOrs()
    // println(cnf)
    val dima = cnf.toDimacs
    //println(dima)
    dima.get.isSatisfiable()
  }
}

case class Dimacs(clauses: Seq[Seq[Int]], varMap:Map[String,Int])
{
  lazy val nbClauses:Int = clauses.size
  lazy val maxVar:Int = clauses.maxBy(c => c.max).max

  def isSatisfiable():Boolean =
  {
    val solver = SolverFactory.newDefault

    solver.newVar(maxVar)
    solver.setExpectedNumberOfClauses(nbClauses)
    // Feed the solver using Dimacs format, using arrays of int
    // (best option to avoid dependencies on SAT4J IVecInt)
    (0 until  nbClauses).foreach(i => {
      val clause = clauses(i).toArray
      solver.addClause(new VecInt(clause))
    })
    solver.isSatisfiable
  }
}

object SimpleSatTest
{
  def example() =
  {
    val p:Proposition = And(
      Or(Literal("p"), (Literal("p"))),
      Or(Literal("q"), ¬(Literal("p")))
    )

    val d:Option[Dimacs] = p.toDimacs

    println(s"$p => $d")
    d.map(_.isSatisfiable())
  }

  def example1(): Unit =
  {
    val p = Literal("p")
    val q = Literal("q")
    val r =  Literal("r")
    val s = Literal("s")
    val phi = (p ∨ q ∧ r) → ¬(s) // {\displaystyle \phi :=((p\lor q)\land r)\to (\neg s)}
  //phi.tseytin
  val simple = phi.simpleCNFConversion()
    println(simple)
    println(simple.flattenOrs().toDimacs)
  }

  def main(args: Array[String]):Unit = example1
}
// case class Implies(p: Proposition, q:Proposition) extends Proposition

/*
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


      case e: ParseFormatException =>

      case e: IOException =>

      case e: ContradictionException =>
        System.out.println("Unsatisfiable (trivial)!")
      case e: TimeoutException =>
        System.out.println("Timeout, sorry!")
    }
  }
}
*/