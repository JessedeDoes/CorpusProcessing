package utils

import java.util.{ArrayList, Comparator}

import org.incava.util.diff.{Diff, Difference}


import scala.collection.JavaConversions._
import scala.xml._
import scala.xml._
import scala.util.matching.Regex._

case class SimOrDiff[T](diff: Option[Difference], sim: Option[Similarity])
{
  lazy val leftStart = if (diff.isEmpty) sim.get.s2 else diff.get.getDeletedStart
  lazy val leftEnd = if (diff.isEmpty) sim.get.s2 + sim.get.length else {
    val de = diff.get.getDeletedEnd
    if (de < 0)
      leftStart
    else
      de + 1}
  lazy val rightStart = if (diff.isEmpty) sim.get.s1 else diff.get.getAddedStart
  lazy val rightEnd = if (diff.isEmpty) sim.get.s1 + sim.get.length else
    {
      val ad = diff.get.getAddedEnd
      if (ad < 0)
        rightStart
      else
       ad + 1}


  override def toString = s"$diff $sim $leftStart-$leftEnd, $rightStart-$rightEnd"
}

case class Similarity(var s1: Int, var s2: Int, var length: Int) //System.err.println("{" + s1 + "," + s2 + ","  + length + "}");
{
}
class AlignmentGeneric[T](c: Comparator[T])  {
  val caseSensitive = true
  var nDifs = 0



  val tokenComparator = c
  var missedBreaks = 0
  var foundBreaks = 0




  private def compareTokenWithOffsetsLists(tokensFromPlainText: List[T], tokensFromXMLFile: List[T]) = {
    val wordDiff = new Diff[T](tokensFromPlainText, tokensFromXMLFile, c)
    val diffs = wordDiff.diff
    val sims = processDiffs(tokensFromPlainText, tokensFromXMLFile, diffs.toList)
    processSimilarities(tokensFromPlainText, tokensFromXMLFile, sims.toList)
    diffs
  }

  def findDiffsAndSimilarities(tokensFromPlainText: List[T], tokensFromXMLFile: List[T]): (List[Difference], List[Similarity]) =
  {
    val wordDiff = new Diff[T](tokensFromPlainText, tokensFromXMLFile, tokenComparator)
    val diffs = wordDiff.diff
    val sims = processDiffs(tokensFromPlainText, tokensFromXMLFile, diffs.toList)
    (diffs.toList,sims.toList)
  }

  private def processSimilarities(tokensFromPlainText: List[T], tokensFromXMLFile: List[T], sims: List[Similarity]) = {
    import scala.collection.JavaConversions._
    for (s <- sims) {
      var i = s.s1
      while ( {
        i < s.s1 + s.length
      }) {
        try {
          val t1 = tokensFromXMLFile.get(i)
          val t2 = tokensFromPlainText.get(s.s2 + (i - s.s1))

          transferTokenWithOffsetsInformation(t1, t2)
          transferAnnotationsToElement(t1)
        } catch {
          case e: Exception =>
            e.printStackTrace()
          //stop=true;
        }

        {
          i += 1;
          i - 1
        }
      }
    }
  }

  protected def transferTokenWithOffsetsInformation(t1: T, t2: T): Unit = {
    //t1.transferredInformation = t2.text
  }

  private def processDiffs(tokensFromPlainText: List[T], tokensFromXMLFile: List[T], diffs: List[Difference]): ArrayList[Similarity] = {
    var end1 = 0
    var end2 = 0
    val sims = new ArrayList[Similarity]

    for (d <- diffs)
    {
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

  protected def transferAnnotationsToElement(t1: T): Unit = {

  }



  private def handleSequence(xmlTokenWithOffsetss: List[T], dbTokenWithOffsetss: List[T]): Boolean = {
    // if (containsText(xmlTokenWithOffsetss) || containsText(dbTokenWithOffsetss)) System.err.println(xmlTokenWithOffsetss + " //// " + dbTokenWithOffsetss)
    if (xmlTokenWithOffsetss.size == dbTokenWithOffsetss.size) {
      var i = 0
      while ( {
        i < xmlTokenWithOffsetss.size
      }) {
        handleSimpleSubstitution(xmlTokenWithOffsetss, dbTokenWithOffsetss, i, i)

        {
          i += 1;
          i - 1
        }
      }
      return true
    }
    else {
      if (false) {
      }
      return false
    }
  }

    def handleSimpleSubstitution(xmlTokenWithOffsetss: List[T], dbTokenWithOffsetss: List[T], i: Int, j: Int): Unit = {
      System.err.println(xmlTokenWithOffsetss + " //S// " + dbTokenWithOffsetss)
      val t1 = xmlTokenWithOffsetss.get(i)
      val t2 = dbTokenWithOffsetss.get(j)
      transferTokenWithOffsetsInformation(xmlTokenWithOffsetss.get(i), dbTokenWithOffsetss.get(j))
      //xmlTokenWithOffsetss.get(i).transferredInformation = dbTokenWithOffsetss.get(j).text;
      //System.err.println(t1.text + " <--> " + t2.text);
      transferAnnotationsToElement(xmlTokenWithOffsetss.get(i))
    }


}

object alignment
{
  case class ElementWithPosition(e: Elem, p: Int)
  {
    override def toString() = s"[$p: ${e.text}/${e \\ "@tpart2b"}/(${e \\ "@__code"}${e \\ "@pos"}) ${e \\ "@id"}]"


  }

  object comp extends Comparator[Char]
  {
    override def compare(t: Char, t1: Char):Int = t.toString.compareToIgnoreCase(t1.toString)
  }


  def alignExpansionWithOriginal(original: String, expansion: String):NodeSeq =
  {
    val a = new AlignmentGeneric[Char](comp)
    val x = "~".r.findAllMatchIn(original).toStream.map(m => m.start).zipWithIndex.map(p => p._1 - p._2)
    val o1 = original.replaceAll("~","")
    val (diffs, sims) = a.findDiffsAndSimilarities(o1.toList, expansion.toList)
    val dPlus  = diffs.map(d => SimOrDiff[Char](Some(d.asInstanceOf[Difference]), None))
    val simPlus  = sims.map(s => SimOrDiff[Char](None, Some(s)))

    val corresp = (dPlus ++ simPlus).sortBy(_.leftStart)
    //Console.err.println(s"[$original] [$expansion]")
    val lr = corresp.map(
      c => {
        //Console.err.println(c)
        val left = o1.substring(c.leftStart, c.leftEnd)
        val right = expansion.substring(c.rightStart, c.rightEnd)
        (left,right,c.leftStart)
      })


    val pieces = lr.flatMap(
      { case (left,right,i) =>
      {
        Console.err.println(s"$left -> $right")
        val K = x.find(k => k >= i && i + left.length() > k)
        val space = if (K.isDefined) "+" else ""
        val spaceSeq = if (space=="") Seq() else Seq(Text(space))
        val leftWithSpace = if (K.isEmpty) left else left.substring(0,K.get-i) + space + left.substring(K.get-i)
        if (left.toLowerCase == right.toLowerCase) Seq(Text(leftWithSpace)) else
        if (left.equals("_"))
          spaceSeq ++ Seq(<expan>{right}</expan>)
        else
          spaceSeq ++ Seq(<choice><orig>{left}</orig><reg>{right}</reg></choice>)
      } }
    )
    pieces
  }

  def main(args: Array[String]) = {
     val tests = List(
       "ap_~kop" -> "apenkop",
       "ap~_kop" -> "apenkop",
       "en_" -> "ende",
       "te~rug" -> "terug",
       "_aap_noot" -> "kaapenaardnoot"
     )
     tests.foreach(x => println(alignExpansionWithOriginal(x._1, x._2)))
  }
}
