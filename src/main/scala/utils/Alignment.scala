package utils

import java.util.{ArrayList, Comparator}

import org.incava.util.diff.{Diff, Difference}


import scala.collection.JavaConversions._
import scala.xml._
import scala.xml._
import scala.util.matching.Regex._

/*
Problem: incava diff is not very good at optimal alignment of small changes, use editdistance-base alignment instead??
 */

case class SimOrDiff[T](diff: Option[Difference], sim: Option[Similarity]) {
  lazy val leftStart: Int = if (diff.isEmpty) sim.get.s2 else diff.get.getDeletedStart
  lazy val leftEnd: Int = if (diff.isEmpty) sim.get.s2 + sim.get.length else {
    val de = diff.get.getDeletedEnd
    if (de < 0)
      leftStart
    else
      de + 1
  }
  lazy val rightStart: Int = if (diff.isEmpty) sim.get.s1 else diff.get.getAddedStart
  lazy val rightEnd: Int = if (diff.isEmpty) sim.get.s1 + sim.get.length else {
    val ad = diff.get.getAddedEnd
    if (ad < 0)
      rightStart
    else
      ad + 1
  }

  def isSimilarity: Boolean = sim.nonEmpty

  override def toString = s"$diff $sim $leftStart-$leftEnd, $rightStart-$rightEnd"
}

case class Similarity(var s1: Int, var s2: Int, var length: Int) //System.err.println("{" + s1 + "," + s2 + ","  + length + "}");
{
}

class Alignment[T](c: Comparator[T]) {
  val caseSensitive = true
  var nDifs = 0


  val tokenComparator = c
  var missedBreaks = 0
  var foundBreaks = 0


  def findDiffsAndSimilarities(tokensFromPlainText: List[T], tokensFromXMLFile: List[T]): (List[Difference], List[Similarity]) = {
    val wordDiff = new Diff[T](tokensFromPlainText, tokensFromXMLFile, tokenComparator)
    val diffs = wordDiff.diff
    val sims = processDiffs(tokensFromPlainText, tokensFromXMLFile, diffs.toList)
    (diffs.toList, sims.toList)
  }

  private def processDiffs(tokensFromPlainText: List[T], tokensFromXMLFile: List[T], diffs: List[Difference]): ArrayList[Similarity] = {
    var end1 = 0
    var end2 = 0
    val sims = new ArrayList[Similarity]

    for (d <- diffs) {
      //System.err.println(d);
      if (d.getAddedStart > end1 && d.getDeletedStart > end2) {
        val s = new Similarity(end1, end2, d.getAddedStart - end1)
        sims.add(s)
      }
      if (d.getAddedEnd >= 0)
        end1 = d.getAddedEnd + 1
      else
        end1 = d.getAddedStart

      if (d.getDeletedEnd >= 0)
        end2 = d.getDeletedEnd + 1
      else
        end2 = d.getDeletedStart

    }
    if (end1 < tokensFromXMLFile.size && end2 < tokensFromPlainText.size)
      sims.add(new Similarity(end1, end2, tokensFromXMLFile.size - end1))
    sims
  }
}

object alignment {

  case class ElementWithPosition(e: Elem, p: Int) {
    override def toString() = s"[$p: ${e.text}/${e \\ "@tpart2b"}/(${e \\ "@__code"}${e \\ "@pos"}) ${e \\ "@id"}]"


  }

  object comp extends Comparator[Char] {
    override def compare(t: Char, t1: Char): Int = t.toString.compareToIgnoreCase(t1.toString)
  }


  def main(args: Array[String]) = {
    val tests = List(
      "ap_~kop" -> "apenkop",
      "ap~_kop" -> "apenkop",
      "en_" -> "ende",
      "te~rug" -> "terug",
      "_aap_noot" -> "kaapenaardnoot"
    )
    //tests.foreach(x => println(alignExpansionWithOriginal(x._1, x._2)))
  }
}
