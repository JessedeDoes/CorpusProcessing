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

  def trueCount(c: T) = truth.values.count(_ == c)
  def guessCount(c: T) = guess.values.count(_ == c)
  def truePositiveCount(c: T) = truth.keySet.filter(k => truth(k) == c && guess(k) == c).size

  def count(a: T, b: T):Int = {
    confusion.get(Item(a, b)) match {
      case Some(i) => i;
      case _ => 0
    }}

  def precision(c: T):Double = {
    val positives = truth.keySet.filter(k => guess(k) == c)
    val truePositives = positives.filter(k => truth(k) == c)
    truePositives.size / positives.size.asInstanceOf[Double]
  }

  def recall(c: T):Double =
  {
    val truePositives = truth.keySet.filter(k => truth(k) == c)
    val positives = truePositives.filter(k => guess(k) == c)
    positives.size / truePositives.size.asInstanceOf[Double]
  }

  def f1(c: T):Double = { val p = precision(c);  val r = recall(c); 2 * p * r / (p + r) }

  lazy val labels:List[T] = confusion.keySet.flatMap(_.both).toList.filter(c => !(c.toString.trim== ""))

  def rowCounts(i: Int):List[Int] = labels.map(s => count(labels(i), s))

  def confusionMatrix(): String =
  {
    val row = (i: Int) => f"${labels(i)}%7s\t" + rowCounts(i).map(j => f"$j%7d").mkString("\t")
    val header = f"${" "}%7s\t" + labels.map(s => f"$s%7s").mkString("\t")
    (header  +: labels.indices.map(row)).mkString("\n")
  }

  def prList():String = labels.map(l => s"$l (${trueCount(l)} ${guessCount(l)} ${truePositiveCount(l)}) -> p:${precision(l)} r:${recall(l)} f1:${f1(l)}").mkString("\n")
}



case class evaluationFromTEI(truthDoc: Elem, guessDoc: Elem)
{
  def getId(n: Node):Option[String] = n.attributes.filter(a => a.prefixedKey.endsWith(":id") ||
    a.key.equals("id")).map(a => a.value.toString).headOption

  val mappings = Map("NP" -> "SPEC_DEELEIGEN", "PV" -> "WW_PV")

  val map: String=> String = s0 =>
    { val s = s0.replace("VW", "VG").replaceAll("^PV", "WW_PV"); if (mappings.contains(s)) mappings(s) else s }

  lazy val guessMap:Map[String,Node] = (guessDoc \\ "w").map(w => getId(w).get->  w).toMap

  val flattenPoS: String => String = p => p.replaceAll("_.*", "")

  def pos(w: Node):String = (w \ "@type").toString
  def ctag(w: Node):String = (w \ "@ctag").toString // cobalts export puts PoS in ctag and possibly multiw in type
  def lemma(w: Node):String = (w \ "@lemma").toString

  def getWord(w: Node):String  = w.text

  def toMap(e: Elem,  f: Node => String, filter: Node => Boolean = n => true):Map[String,String] =
    (e \\ "w").filter(filter).map(w => getId(w).get -> f(w)).toMap

  def isMulti(n: Node):Boolean = "multiw".equals((n \ "@type").toString) || ctag(n).contains("|")

  def getEvaluation(truthFeature: Node=>String, guessFeature: Node=>String, truthFilter: Node=>Boolean = n => true):SimpleEvaluation[String,String] =
  {
    val truthMap = toMap(truthDoc, truthFeature, truthFilter)
    val guessMap = toMap(guessDoc, guessFeature)
    SimpleEvaluation(truthMap, guessMap)
  }

  def printTokens = (truthDoc \\ "w").foreach(
    w => {
      val id = getId(w).get
      val multi = if (isMulti(w)) "M" else "S"
      val word = getWord(w)
      val tLem = lemma(w)
      val gLem = lemma(guessMap(id))
      val tPos = map(ctag(w))
      val gPos = if (guessMap.contains(id)) pos(guessMap(id)) else "MISSING"
      val OK = tPos == gPos
      val flatOK = flattenPoS(tPos) == flattenPoS(gPos)
      println(s"$word\t$multi\t($tPos, $gPos, $flatOK, $OK)\t($tLem, $gLem, ${tLem == gLem})\t$flatOK\t$OK")
    }
  )

  lazy val lemEvalFiltered:SimpleEvaluation[String, String] = getEvaluation(lemma, lemma, !isMulti(_))
  lazy val posEvalFiltered:SimpleEvaluation[String, String] = getEvaluation(map.compose(ctag), pos, !isMulti(_))
  lazy val flatEvalFiltered:SimpleEvaluation[String, String] = getEvaluation(flattenPoS.compose(map.compose(ctag)), flattenPoS.compose(pos), !isMulti(_))

  lazy val lemEvalUnfiltered = getEvaluation(lemma, lemma)
  lazy val posEvalUnfiltered = getEvaluation(map.compose(ctag), pos)
  lazy val flatEvalUnfiltered = getEvaluation(flattenPoS.compose(map.compose(ctag)), flattenPoS.compose(pos))

  lazy val posAccuracyFiltered:Double = posEvalFiltered.accuracy
  lazy val flatAccuracyFiltered:Double = flatEvalFiltered.accuracy
  lazy val lemAccuracyFiltered:Double = lemEvalFiltered.accuracy

  lazy val posAccuracyUnFiltered:Double = posEvalUnfiltered.accuracy
  lazy val flatAccuracyUnFiltered:Double = flatEvalUnfiltered.accuracy
  lazy val lemAccuracyUnFiltered:Double = lemEvalUnfiltered.accuracy

  lazy val report =
    s"""
       Met multiwords: PoS: Flat: ${flatAccuracyUnFiltered} Full: ${posAccuracyUnFiltered}; Lemma: ${lemAccuracyUnFiltered}
       Zonder multiwords: PoS: Flat: ${flatAccuracyFiltered} Full: ${posAccuracyFiltered}; Lemma: ${lemAccuracyFiltered}
     """
}

object NederlabEval
{
  // truth is arg 0
  def main(args: Array[String]):Unit =
  {
    val e = evaluationFromTEI(XML.load(args(0)), XML.load(args(1)))
    e.printTokens
    println(e.report)
    println(e.flatEvalFiltered.confusionMatrix)
    println(e.flatEvalFiltered.prList())
  }
}
