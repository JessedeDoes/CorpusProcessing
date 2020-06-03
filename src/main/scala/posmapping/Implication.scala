package posmapping
import scala.xml._
import scala.util.{Try, Success, Failure}

case class Implication(antecedent: Tag => Boolean, consequent: Tag => Boolean, description: String, isDeclaration: Boolean=true,
                       fix: Option[Tag => Tag]  = None) {
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

  def addFeature(t: Tag, n: String, v: String) : Tag = {
    t match {
      case t1: CHNStyleTag =>
        if (t1.features.exists(_.name == n)) t1 else {
          val s = t1.toString
          val add = if (s.contains("=")) s",$n=$v" else s"$n=$v"
          val s1 = s.replaceAll("\\)", add + ")")
          new CHNStyleTag(s1, t1.tagset)
        }

    }
  }

  def addFeatures(t: Tag, m: Map[String,String])  = {
        m.toList.foldLeft(t)({case (t,(n,v)) => addFeature(t,n,v)})
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
      case Success(f) => Implication(f._1, f._2, s"$f", false)
    }
  }

  def declarationFromText(s: String) = {
    Try ( {
      val x = s.trim.split("\\s*(=>|\\|\\|)\\s*");
      val antecedent = parseFeatures(x(0))
      val consequent  = x(1).split("\\s*,\\s*").toSet
      val fix: Option[Tag => Tag] = if (x.size <= 2) None else {
        val addition = parseFeatures(x(2))
        Some(t => addFeatures(t,addition))
      }
      val z = (antecedent, consequent, fix)
      // Console.err.println(z)
      // System.exit(1)
      z

    }) match {
      case Success(f) => Implication(f._1, f._2, s"$f", fix=f._3)
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



