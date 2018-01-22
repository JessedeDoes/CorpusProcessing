package evaluation

import scala.xml._

case class Item[T](truth: T, guess: T)
{
  def toSet: Set[T] = Set(truth,guess)
}

case class SimpleEvaluation[S,T](truth: Map[S,T], guess: Map[S,T])
{
  lazy val accuracy:Double = truth.keySet.count(k => truth(k) == guess(k)) / truth.keySet.size.asInstanceOf[Double]

  lazy val confusion:Map[Item[T],Int]  = truth.map({case (k,v) => Item(truth(k), guess(k))} ).groupBy(identity).mapValues(l => l.size)

  def trueCount(c: T):Int = truth.values.count(_ == c)
  def guessCount(c: T):Int = guess.values.count(_ == c)
  def truePositiveCount(c: T):Int = truth.keySet.count(k => truth(k) == c && guess(k) == c)

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

  lazy val labels:List[T] = confusion.keySet.flatMap(_.toSet).toList.filter(c => !(c.toString.trim== ""))

  def rowCounts(i: Int):List[Int] = labels.map(s => count(labels(i), s))

  def confusionMatrix(): String =
  {
    val row = (i: Int) => f"${labels(i)}%7s\t" + rowCounts(i).map(j => f"$j%7d").mkString("\t")
    val header = f"${" "}%7s\t" + labels.map(s => f"$s%7s").mkString("\t")
    (header  +: labels.indices.map(row)).mkString("\n")
  }

  def prList():String =
    labels.map(l => s"$l (${trueCount(l)} ${guessCount(l)} ${truePositiveCount(l)}) -> p:${precision(l)} r:${recall(l)} f1:${f1(l)}")
      .mkString("\n")

  def confusionsByFrequency():List[String] = confusion.toList
    .filter({ case (Item(t,g), k) => t != g})
    .sortBy({ case (_, k) => -1 * k})
    .map({ case (Item(t,g), k) => s"$t->$g: $k"})
}



case class evaluationFromTEI(truthDoc: NodeSeq, guessDoc: NodeSeq, setName: String)
{
  def getId(n: Node):Option[String] = n.attributes.filter(a => a.prefixedKey.endsWith(":id") ||
    a.key.equals("id")).map(a => a.value.toString).headOption

  val mappings = Map("NP" -> "SPEC_DEELEIGEN", "PV" -> "WW_PV", "AJ" -> "ADJ")
  val decentPoS = Set("N", "WW", "LID", "TW", "VG", "VZ", "TSW", "ADJ", "BW", "SPEC", "LET", "VNW")

  val map: String=> String = s0 =>
  { val s = s0.replace("VW", "VG").replaceAll("^PV", "WW_PV"); if (mappings.contains(s)) mappings(s) else s }

  lazy val guessMap:Map[String,Node] = (guessDoc \\ "w").map(w => getId(w).get->  w).toMap

  val flattenPoS: String => String = p => p.replaceAll("_.*", "")

  def getPoS(w: Node):String = (w \ "@type").toString
  def getPoSFromCtag(w: Node):String = (w \ "@ctag").toString // cobalts export puts PoS in ctag and possibly multiw in type
  def lemma(w: Node):String = (w \ "@lemma").toString

  def getWord(w: Node):String  = w.text

  def toMap(e: NodeSeq,  f: Node => String, filter: Node => Boolean = n => true):Map[String,String] =
    (e \\ "w").filter(filter).map(w => getId(w).get -> f(w)).toMap

  def isMulti(n: Node):Boolean = "multiw".equals((n \ "@type").toString) || getPoSFromCtag(n).contains("|") || getPoSFromCtag(n).contains("+")

  def isSimpleWord(n:Node) = !isMulti(n) && decentPoS.contains(flattenPoS(getPoSFromCtag(n)))

  def getEvaluation(truthFeature: Node=>String, guessFeature: Node=>String, truthFilter: Node=>Boolean = n => true):SimpleEvaluation[String,String] =
  {
    val truthMap = toMap(truthDoc, truthFeature, truthFilter)
    val guessMap = toMap(guessDoc, guessFeature)
    SimpleEvaluation(truthMap, guessMap)
  }

  def printTokens = (truthDoc \\ "w").map(
    w => {
      val id = getId(w).get
      val multi = if (isMulti(w)) "M" else "S"
      val word = getWord(w)
      val tLem = lemma(w)
      val gLem = lemma(guessMap(id))
      val tPos = map(getPoSFromCtag(w))
      val gPos = if (guessMap.contains(id)) getPoS(guessMap(id)) else "MISSING"
      val OK = tPos == gPos
      val flatOK = flattenPoS(tPos) == flattenPoS(gPos)
      s"$word\t$multi\t($tPos, $gPos, $flatOK, $OK)\t($tLem, $gLem, ${tLem == gLem})"
    }
  ).mkString("\n")

  lazy val lemEvalFiltered:SimpleEvaluation[String, String] = getEvaluation(lemma, lemma, isSimpleWord)
  lazy val posEvalFiltered:SimpleEvaluation[String, String] = getEvaluation(map.compose(getPoSFromCtag), getPoS, isSimpleWord)
  lazy val flatEvalFiltered:SimpleEvaluation[String, String] =
    getEvaluation(flattenPoS.compose(map.compose(getPoSFromCtag)), flattenPoS.compose(getPoS), isSimpleWord)

  lazy val lemEvalUnfiltered:SimpleEvaluation[String, String] = getEvaluation(lemma, lemma)
  lazy val posEvalUnfiltered:SimpleEvaluation[String, String] = getEvaluation(map.compose(getPoSFromCtag), getPoS)
  lazy val flatEvalUnfiltered:SimpleEvaluation[String, String] =
    getEvaluation(flattenPoS.compose(map.compose(getPoSFromCtag)), flattenPoS.compose(getPoS))

  lazy val posAccuracyFiltered:Double = posEvalFiltered.accuracy
  lazy val flatAccuracyFiltered:Double = flatEvalFiltered.accuracy
  lazy val lemAccuracyFiltered:Double = lemEvalFiltered.accuracy

  lazy val posAccuracyUnFiltered:Double = posEvalUnfiltered.accuracy
  lazy val flatAccuracyUnFiltered:Double = flatEvalUnfiltered.accuracy
  lazy val lemAccuracyUnFiltered:Double = lemEvalUnfiltered.accuracy
  lazy val nTokens:Int = (truthDoc \\ "w").size
  lazy val nMultiwords:Int = (truthDoc \\ "w").count(isMulti)

  lazy val shortReport =
    s"""
#### Tokens: $nTokens, multiwords:  $nMultiwords
Met multiwords: PoS: Flat: ${flatAccuracyUnFiltered} Full: ${posAccuracyUnFiltered}; Lemma: ${lemAccuracyUnFiltered}
Zonder multiwords: PoS: Flat: ${flatAccuracyFiltered} Full: ${posAccuracyFiltered}; Lemma: ${lemAccuracyFiltered}
     """

  lazy val fullReport =
    s"""
       |${printTokens}
       |$shortReport
       |${flatEvalFiltered.confusionMatrix()}
       |* precisie, recall, f1 voor hoofdwoordsoort *
       |${flatEvalFiltered.prList()}
       |* top verwarringen voor hoofdwoordsoort *
       |${flatEvalFiltered.confusionsByFrequency().take(10).mkString("\n")}
       |* top verwarringen voor lemma *
       |${lemEvalFiltered.confusionsByFrequency().take(50).mkString("\n")}
     """.stripMargin

  def p(d: Double) = f"${100*d}%2.2f"

  lazy val HTMLReport:NodeSeq =
    <tr>
      <td>{setName}</td>
      <td>{nTokens}</td>
      <td>{p(flatAccuracyUnFiltered)}</td>
      <td>{p(posAccuracyUnFiltered)}</td>
      <td>{p(lemAccuracyUnFiltered)}</td>
    </tr>
      <tr>
        <td>{setName} - without multiwords/clitics</td>
        <td>{nTokens - nMultiwords}</td>
        <td>{p(flatAccuracyFiltered)}</td>
        <td>{p(posAccuracyFiltered)}</td>
        <td>{p(lemAccuracyFiltered)}</td>
      </tr>
}

object NederlabEval
{
  // truth is arg 0
  def addElems(a: Seq[Node], b: Elem):Seq[Node]  = a ++ Seq(b)

  def evaluateSet(setFileName: String): evaluationFromTEI =
  {
    val pairs = scala.io.Source.fromFile(setFileName).getLines.map(l => l.split("\\s+")).filter(r => r.size == 2).toList
    val truth:NodeSeq = pairs.map(r => XML.load(r(0))).foldLeft(Seq.empty[Node])(addElems)
    val guess:NodeSeq = pairs.map(r => XML.load(r(1))).foldLeft(Seq.empty[Node])(addElems)
    evaluationFromTEI(truth,guess, setFileName.replaceAll(".*/",""))
  }

  def main(args: Array[String]):Unit =
  {
    val e =
      if (args.size == 2 && args(0).endsWith(".xml"))
        println(evaluationFromTEI(XML.load(args(0)), XML.load(args(1)), args(0)).fullReport)
      else if (args.size == 1)
        println(evaluateSet(args(0)).fullReport)
      else
      {
        val report = <table border="border" style="border-style:solid; border-collapse: collapse">
          <tr><td>Set</td> <td>size</td> <td>Main PoS</td> <td>Full eval PoS</td> <td>Lemma</td></tr>
          {args.map(a => evaluateSet(a).HTMLReport)}
        </table>
        println(report)
      }
  }
}
