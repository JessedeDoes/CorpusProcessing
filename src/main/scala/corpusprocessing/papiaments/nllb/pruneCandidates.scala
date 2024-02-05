package corpusprocessing.papiaments.nllb

import java.io.{BufferedInputStream, FileInputStream, PrintWriter}
import java.util.zip.GZIPInputStream

object pruneCandidates {
  val pairs = "/mnt/Projecten/Papiaments/Alignments/pairs.out.gz"
  def list(words: List[(String,Int)])  = words.map({case (w,i) => s"$w: $i"}).mkString(";")
  def main(args: Array[String])  = {
     val p = pruneCandidates(args.headOption.getOrElse(pairs))
     val out_e2p = new PrintWriter("/tmp/pairs.pruned.e2p.out")
     val out_p2e = new PrintWriter("/tmp/pairs.pruned.p2e.out")

     p.leftWords.foreach(w => {
       // Console.err.println(w.word)
       out_e2p.println(s"${w.word}:${w.count} ${list(w.prunedCandidates)} -unpruned- ${list(w.candidates.take(2))}")
     })
     out_e2p.close()

     p.rightWords.foreach(w => {
      // Console.err.println(w.word)
      out_p2e.println(s"${w.word}:${w.count} ${list(w.prunedCandidates)} -unpruned- ${list(w.candidates.take(2))}")
     })
     out_e2p.close()
     out_p2e.close()
  }
}


case class Word(theOtherSide:Mapped, word: String, candidates: List[(String,Int)]) {

  val minPart = 10
  lazy val count = candidates.map(_._2).sum
  def prunedCandidates = candidates.filter(x => {
    val otherCount = theOtherSide.count(x._1)
    val c = x._2
    minPart * c > otherCount && minPart * otherCount > this.count
  })

  def prunedCandidates2 = candidates.zipWithIndex.filter({case (x,i) => {
    val otherCount = theOtherSide.count(x._1)
    val c = x._2
    val backwards = theOtherSide.backwards(x._1)
    10 * c > otherCount &&
      i < 5 &&
      backwards.indices.find(i => backwards(i)._1 == this.word)
        .map(i => i < 5).getOrElse(false)
  }}).map(_._1)
}

case class Mapped(words: Seq[Word]) {
  lazy val map = words.map(w => w.word -> w).toMap
  def count(s: String) = map.get(s).map(_.count).getOrElse(0)
  def backwards(s: String): Seq[(String, Int)] = map.get(s).map(_.candidates).getOrElse(List())
}
case class pruneCandidates(fileName: String)  {

  def getSource(f: String) = if (f.endsWith(".gz")) io.Source.fromInputStream(new GZIPInputStream(new BufferedInputStream(new FileInputStream(f)))) else io.Source.fromFile(f)

  def filter(t: (String,String,Int))  = t._1.toLowerCase == t._1 && t._2.toLowerCase == t._2 &&
    t._1.replaceAll("\\p{P}","") == t._1 && t._2.replaceAll("\\p{P}","") == t._2 &&
    t._1.replaceAll("[0-9]","") == t._1 && t._2.replaceAll("[0-9]","") == t._2

  lazy val candidates: Seq[(String, String, Int)] = getSource(fileName).getLines.map(l => l.split("\t")).map(a =>
    (a(0), a(1), a(2).toInt)
  ).toList.filter(filter)

  val nullMap = Mapped(Seq())

  lazy val leftWords0: Seq[Word] =
    candidates.groupBy(_._1)
      .map({case (w,l) => Word(nullMap, w, l.map(x => x._2 -> x._3).sortBy(-1 * _._2).toList)})
      .toList.sortBy(-1 * _.count)
  lazy val rightWords0: Seq[Word] =  candidates.groupBy(_._2).map({case (w,l) => Word(nullMap, w, l.map(l => l._1 -> l._3).sortBy(-1 * _._2).toList)}).toList.sortBy(-1 * _.count)

  val leftWordMap = Mapped(leftWords0)
  val rightWordMap = Mapped(rightWords0)

  val leftWords = leftWords0.map(_.copy(theOtherSide = rightWordMap))
  val rightWords = rightWords0.map(_.copy(theOtherSide = leftWordMap))
}
