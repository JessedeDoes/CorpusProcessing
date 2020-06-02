package posmapping
import scala.xml._
import scala.util.{Try, Success, Failure}

case class Implication(antecedent: Tag => Boolean, consequent: Tag => Boolean, description: String) {
  def apply(t: Tag) = !antecedent(t) || consequent(t)

  override def toString: String = description
}

object Implication {

  implicit def valueFilter(m: Map[String, String]): Tag => Boolean = {
    t => {
      m.forall({case (n,v) =>  t.features.exists(f => f.name == n && f.value == v)})
    }
  }

  implicit def nameFilter(m: Set[String]): Tag => Boolean = {
    t => {
      m.forall({case n =>  t.features.exists(f => f.name == n)})
    }
  }

  val m = Map("pos" -> "NOU-C")

  val noun_has_number = Implication(m, Set("number"), "NOU has number")

  def parseFeatures(s: String) = {
    val featureValues:Array[String] = s.split("\\s*,\\s*").filter(f => !(f.trim == ""))
    def parseFeature(f: String) = {
      Try( { val x = f.split("="); x(0) -> x(1) }) match
      {
        case Success(f) => f
        case _ => "unk" -> "unk"
      }
    }
    featureValues.map(parseFeature).toMap
  }

  def implicationFromText(s: String) = {
    Try ( { val x = s.trim.split("\\s*=>\\s*"); parseFeatures(x(0)) -> parseFeatures(x(1)) }) match {
      case Success(f) => Implication(f._1, f._2, s"$f")
    }
  }

  def declarationFromText(s: String) = {
    Try ( { val x = s.trim.split("\\s*=>\\s*"); parseFeatures(x(0)) -> x(1).split("\\s*,\\s*").toSet }) match {
      case Success(f) => Implication(f._1, f._2, s"$f")
    }
  }

  def fromXML(z: Elem) = {
    (z \\ "implication").map(x => implicationFromText(x.text)) ++ (z \\ "declaration").map(x => declarationFromText(x.text))
  }

  def main(args: Array[String]): Unit = {
    val i = TagsetDiachroonNederlands.TDNTagset.implications
    println(i)
    println(implicationFromText("pos=NOU-C=>number=sg"))
    println(declarationFromText("pos=NOU-C=>number"))
  }
}



