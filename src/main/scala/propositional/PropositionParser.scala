package propositional

import scala.util.parsing.combinator.JavaTokenParsers


object PropositionParser extends App with PropositionParser
{
  def parse(p: String) = parseAll(program, p)
  def parseFile(f: String) = parse(scala.io.Source.fromFile(f).getLines()
    .map(s => s.replaceAll("#.*", ""))
    .filter(s => !(s.trim == ""))
    .mkString(", "))

  // println(parseAll(program, "p ∨ q → ¬(cgn:pos=ADJ|NOUN)").get)
}

trait PropositionParser extends JavaTokenParsers {

  lazy val program: Parser[Proposition] =
    (clause ~ rep(",".r ~ clause)) ^^ {
      case e ~ es => es.foldLeft(e) {
        case (t1, "," ~ t2) => t1 ∧ t2
      }
    }

  lazy val clause: Parser[Proposition] =
    (expression ~ rep("[→↔]|->|<->".r ~ expression)) ^^ {
      case e ~ es => es.foldLeft(e) {
        case (t1, "→" ~ t2) => t1 → t2
        case (t1, "->" ~ t2) => t1 → t2

        case (t1, "↔" ~ t2) => t1 ↔ t2
        case (t1, "<->" ~ t2) => t1 ↔ t2
      }
  }

  lazy val expression: Parser[Proposition] =
    term ~ rep("[∨|]".r ~ term)  ^^ {
      case t ~ ts => ts.foldLeft(t) {
        case (t1, "∨" ~ t2) => Or(t1, t2)
        case (t1, "|" ~ t2) => propositional.Or(t1, t2)
      }
    }

  lazy val term: Parser[Proposition] =
    factor ~ rep("[∧&]".r ~ factor) ^^ {
      case t ~ ts => ts.foldLeft(t) {
        case (t1, "∧" ~ t2) => And(t1, t2)
        case (t1, "&" ~ t2) => propositional.And(t1, t2)
      }
    }

  lazy val factor: Parser[Proposition] =
    "(" ~> clause <~ ")" | literal | notFactor

  lazy val notFactor: Parser[Proposition] = "[¬!]".r ~ factor ^^ {
    case not ~ c => Not(c)
  }

  lazy val literal: Parser[Proposition] = "[A-Za-z][A-za-z[0-9]_=:|.-]*".r ^^ { case s => Literal(s) }
}

// example combinator parser: https://gist.github.com/sschaef/5529436