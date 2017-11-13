package sat

import scala.util.parsing.combinator.JavaTokenParsers

object PropositionParser extends App with PropositionParser
{
  def parse(p: String) = parseAll(program, p)
  def parseFile(f: String) = parse(scala.io.Source.fromFile(f).getLines().mkString(","))

  // println(parseAll(program, "p ∨ q → ¬(pos=ADJ)").get)
}

trait PropositionParser extends JavaTokenParsers {

  lazy val program: Parser[Proposition] =
    (clause ~ rep(",".r ~ clause)) ^^ {
      case e ~ es => es.foldLeft(e) {
        case (t1, "→" ~ t2) => t1 ∧ t2
      }
    }

  lazy val clause: Parser[Proposition] =
    (expr ~ rep("[→↔]".r ~ expr)) ^^ {
      case e ~ es => es.foldLeft(e) {
        case (t1, "→" ~ t2) => t1 → t2
      }
  }

  lazy val expr: Parser[Proposition] =
    term ~ rep("[∨]".r ~ term)  ^^ {
      case t ~ ts => ts.foldLeft(t) {
        case (t1, "∨" ~ t2) => Or(t1, t2)
      }
    }

  lazy val term: Parser[Proposition] =
    factor ~ rep("[∧]".r ~ factor) ^^ {
      case t ~ ts => ts.foldLeft(t) {
        case (t1, "∧" ~ t2) => And(t1, t2)
      }
    }

  lazy val factor: Parser[Proposition] =
    "(" ~> clause <~ ")" | literal | notFactor

  lazy val notFactor: Parser[Proposition] = "¬".r ~ factor ^^ {
    case not ~ c => Not(c)
  }

  lazy val literal: Parser[Proposition] = "[A-za-z[0-9]_=]+".r ^^ { case s => Literal(s) }
}

/*
trait ExprParsers2 extends JavaTokenParsers {

  sealed abstract class Tree
  case class Add(t1: Tree, t2: Tree) extends Tree
  case class Sub(t1: Tree, t2: Tree) extends Tree
  case class Mul(t1: Tree, t2: Tree) extends Tree
  case class Div(t1: Tree, t2: Tree) extends Tree
  case class Num(t: Double) extends Tree

  def eval(t: Tree): Double = t match {
    case Add(t1, t2) => eval(t1)+eval(t2)
    case Sub(t1, t2) => eval(t1)-eval(t2)
    case Mul(t1, t2) => eval(t1)*eval(t2)
    case Div(t1, t2) => eval(t1)/eval(t2)
    case Num(t) => t
  }

  lazy val expr: Parser[Tree] = term ~ rep("[+-]".r ~ term) ^^ {
    case t ~ ts => ts.foldLeft(t) {
      case (t1, "+" ~ t2) => Add(t1, t2)
      case (t1, "-" ~ t2) => Sub(t1, t2)
    }
  }

  lazy val term = factor ~ rep("[*]".r ~ factor) ^^ {
    case t ~ ts => ts.foldLeft(t) {
      case (t1, "*" ~ t2) => Mul(t1, t2)
      case (t1, "/" ~ t2) => Div(t1, t2)
    }
  }

  lazy val factor = "(" ~> expr <~ ")" | num

  lazy val num = floatingPointNumber ^^ { t => Num(t.toDouble) }
}

*/