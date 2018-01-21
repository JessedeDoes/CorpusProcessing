package evaluation

import scala.xml._

case class Item[T](truth: T, guess: T)

case class SimpleEvaluation[S,T](truth: Map[S,T], guess: Map[S,T])
{
  lazy val accuracy:Double = truth.keySet.count(k => truth(k) == guess(k)) / truth.keySet.size.asInstanceOf[Double]
  lazy val confusion:Map[Item[T],Int]  = truth.keySet.map(k => Item(truth(k), guess(k))).groupBy(identity).mapValues(l => l.size)

  def count(a: T, b: T):Int = {
    confusion.get(Item(a, b)) match {
      case Some(i) => i;
      case _ => 0
    }}

  def confusionMatrix(): String =
  {
    val labels:List[T] = confusion.keySet.flatMap({ case (Item(a:T, b:T), _) => Set[T](a, b) }).toSet.toList
    def rowCounts(i: Int) = labels.map(s => count(labels(i), s))

    val row = (i: Int) => f"${labels(i)}%20s\t" + rowCounts(i).map(j => f"$j%20d").mkString("\t")
    val header = f"${" "}%20s\t" + labels.map(s => f"$s%20s").mkString("\t")
    (header  +: labels.indices.map(row)).mkString("\n")
  }
}

case class evaluationFromTEI(truthDoc: Elem, guessDoc: Elem)
{
  def getId(n: Node):Option[String] = n.attributes.filter(a => a.prefixedKey.endsWith(":id") ||
    a.key.equals("id")).map(a => a.value.toString).headOption

  def toPosMap(e: Elem):Map[String,String] = (e \\ "w").map(w => getId(w).get ->  (w \ "@type").text).toMap
  def toLemMap(e: Elem):Map[String,String] = (e \\ "w").map(w => getId(w).get ->  (w \ "@lemma").text).toMap

  private lazy val truePoSMap = toPosMap(truthDoc)
  private lazy val guessPosMap = toPosMap(guessDoc)

  lazy val posEval:SimpleEvaluation[String,String] = SimpleEvaluation(truePoSMap, guessPosMap)

  lazy val posAccuracy:Double = posEval.accuracy
}

object NederlabEval
{
  // truth is arg 0
  def main(args: Array[String]):Unit =
  {
     val e = evaluationFromTEI(XML.load(args(0)), XML.load(args(1)))
     println(e.posAccuracy)
  }
}
