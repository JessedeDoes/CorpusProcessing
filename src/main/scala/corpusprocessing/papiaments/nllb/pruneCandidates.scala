package corpusprocessing.papiaments.nllb

import java.io.PrintWriter

object pruneCandidates {

  def list(words: List[(String,Int)])  = words.map({case (w,i) => s"$w: $i"}).mkString(";")
  def main(args: Array[String])  = {
     val p = pruneCandidates("/tmp/pairs.out")
     val out = new PrintWriter("/tmp/pairs.pruned.out")
     p.leftWords.foreach(w => {
       Console.err.println(w.word)
       out.println(s"${w.word}:${w.count} ${list(w.prunedCandidates)} -unpruned- ${list(w.candidates.take(2))}")
     })
    out.close()
  }
}


case class Word(theOtherSide:Mapped, word: String, candidates: List[(String,Int)]) {
  lazy val count = candidates.map(_._2).sum
  def prunedCandidates = candidates.filter(x => {
    val otherCount = theOtherSide.count(x._1)
    val c = x._2
    5 * c > otherCount && 5 * otherCount > this.count
  })

  def prunedCandidates2 = candidates.filter(x => {
    val otherCount = theOtherSide.count(x._1)
    val backwards = theOtherSide.backwards(x._1)
    backwards.indices.find(i => backwards(i)._1 == this.word).map(i => i < 5).getOrElse(false)
  })
}

case class Mapped(words: Seq[Word]) {
  lazy val map = words.map(w => w.word -> w).toMap
  def count(s: String) = map.get(s).map(_.count).getOrElse(0)
  def backwards(s: String): Seq[(String, Int)] = map.get(s).map(_.candidates).getOrElse(List())
}
case class pruneCandidates(fileName: String)  {
  val candidates  = io.Source.fromFile(fileName).getLines.map(l => l.split("\t")).map(a =>
    (a(0), a(1), a(2).toInt)
  ).toList
  val nullMap = Mapped(Seq())
  lazy val leftWords0: Seq[Word] = { Console.err.println(candidates.size); candidates.groupBy(_._1).map({case (w,l) => Word(nullMap, w, l.map(l => l._2 -> l._3).sortBy(-1 * _._2))}).toList.sortBy(-1 * _.count) }
  lazy val rightWords0: Seq[Word] = { Console.err.println(candidates.size); candidates.groupBy(_._2).map({case (w,l) => Word(nullMap, w, l.map(l => l._1 -> l._3).sortBy(-1 * _._2))}).toList.sortBy(-1 * _.count) }

  val leftWordMap = Mapped(leftWords0)
  val rightWordMap = Mapped(rightWords0)

  val leftWords = leftWords0.map(_.copy(theOtherSide = rightWordMap))
  val rightWords = rightWords0.map(_.copy(theOtherSide = leftWordMap))
}
