package evaluation

import scala.xml._

case class Item[T](truth: T, guess: T)
{
  def both: Set[T] = Set(truth,guess)
}

case class SimpleEvaluation[S,T](truth: Map[S,T], guess: Map[S,T])
{
  lazy val accuracy:Double = truth.keySet.count(k => truth(k) == guess(k)) / truth.keySet.size.asInstanceOf[Double]

  lazy val confusion:Map[Item[T],Int]  = truth.map({case (k,v) => Item(truth(k), guess(k))} ).groupBy(identity).mapValues(l => l.size)

  def count(a: T, b: T):Int = {
    confusion.get(Item(a, b)) match {
      case Some(i) => i;
      case _ => 0
    }}

  def confusionMatrix(): String =
  {
    val labels:List[T] = confusion.keySet.flatMap(_.both).toSet.toList
    def rowCounts(i: Int) = labels.map(s => count(labels(i), s))

    val row = (i: Int) => f"${labels(i)}%7s\t" + rowCounts(i).map(j => f"$j%7d").mkString("\t")
    val header = f"${" "}%7s\t" + labels.map(s => f"$s%7s").mkString("\t")
    (header  +: labels.indices.map(row)).mkString("\n")
  }
}

case class evaluationFromTEI(truthDoc: Elem, guessDoc: Elem)
{
  def getId(n: Node):Option[String] = n.attributes.filter(a => a.prefixedKey.endsWith(":id") ||
    a.key.equals("id")).map(a => a.value.toString).headOption

  lazy val guessMap = (guessDoc \\ "w").map(w => getId(w).get->  w).toMap

  val flatten: String=>String =  p =>  p.replaceAll("_.*", "")

  def pos(w: Node):String = (w \ "@type").toString
  def ctag(w: Node):String = (w \ "@ctag").toString

  def getWord(w: Node)  = w.text

  def toMap(e: Elem, f: Node => String)  = (e \\ "w").map(w => getId(w).get -> f(w)).toMap

  def toPosMap(e: Elem):Map[String,String] = (e \\ "w").map(w => getId(w).get ->  (w \ "@type").text).toMap
  def toLemMap(e: Elem):Map[String,String] = (e \\ "w").map(w => getId(w).get ->  (w \ "@lemma").text).toMap

  private lazy val truePoSMap = toMap(truthDoc, ctag)
  private lazy val guessPosMap = toMap(guessDoc, pos)


  private lazy val trueFlatPoSMap = toMap(truthDoc, flatten.compose(ctag))
  private lazy val guessFlatPosMap = toMap(guessDoc, flatten.compose(pos))

  def compare = (truthDoc \\ "w").foreach(
    w => {
      val id = getId(w).get
      val word = getWord(w)
      val tPos = ctag(w)
      val gPos = pos(guessMap(id))
      val OK = tPos == gPos
      val flatOK =  flatten(tPos) == flatten(gPos)
      println(s"$word\t$tPos\t$gPos\t$flatOK\t$OK")
    }
  )

  lazy val posEval:SimpleEvaluation[String,String] = SimpleEvaluation(truePoSMap, guessPosMap)
  lazy val flatEval:SimpleEvaluation[String,String] = SimpleEvaluation(trueFlatPoSMap, guessFlatPosMap)
  lazy val posAccuracy:Double = posEval.accuracy
  lazy val flatAccuracy:Double = flatEval.accuracy
}

object NederlabEval
{
  // truth is arg 0
  def main(args: Array[String]):Unit =
  {
    val e = evaluationFromTEI(XML.load(args(0)), XML.load(args(1)))
    e.compare
    println(s"Flat: ${e.flatAccuracy} Full: ${e.posAccuracy}")
    println(e.flatEval.confusionMatrix)
  }
}
