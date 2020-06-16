package utils

import java.util.{ArrayList, Comparator}


import org.incava.util.diff.{Diff, Difference}
import utils.TokenizerWithOffsets._

import scala.collection.JavaConversions._
import scala.xml._

import AlignmentGeneric.Similarity

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

  def showElements(l1: List[T], l2: List[T]): String =
  {
    val s1 = l1.slice(leftStart, leftEnd)
    val s2 = l2.slice(rightStart, rightEnd)

    s"|${s1.mkString("␣")}| ➜➜ |${s2.mkString("␣")}|"
  }
}

object AlignmentGeneric
{
  case class Similarity(var s1: Int, var s2: Int, var length: Int) //System.err.println("{" + s1 + "," + s2 + ","  + length + "}");
  {
  }
}

class AlignmentGeneric[T](c: Comparator[T])  {
  val caseSensitive = true
  var nDifs = 0



  val tokenComparator = c
  var missedBreaks = 0
  var foundBreaks = 0


  def findChunks(tokensFromPlainText: List[T], tokensFromXMLFile: List[T]): List[SimOrDiff[T]] =
  {
    val (diffs, sims) = findDiffsAndSimilarities(tokensFromPlainText, tokensFromXMLFile)
    val dPlus = diffs.map(d => SimOrDiff[T](Some(d.asInstanceOf[Difference]), None))
    val simPlus = sims.map(s => SimOrDiff[T](None, Some(s)))
    val corresp = (dPlus ++ simPlus).sortBy(_.leftStart)
    corresp
  }

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

  /*
     if (d.getDeletedEnd >= 0 || d.getAddedEnd >= 0)
     {
       val de = d.getDeletedEnd
       ds = d.getDeletedStart
       ae = d.getAddedEnd
       val xmlTokenWithOffsetss = new ArrayList[TokenWithOffsets]
       val plainTextTokenWithOffsetss = new ArrayList[TokenWithOffsets]
       if (ae >= 0) {
         as = d.getAddedStart
         var j = as
         while ( {
           j <= ae
         }) {
           try {
             val w = tokensFromXMLFile.get(j)
             xmlTokenWithOffsetss.add(w)
           } catch {
             case e: Exception =>
               e.printStackTrace()
           }

           {
             j += 1;
             j - 1
           }
         }
       }
       var i = ds
       while ( {
         i <= de
       }) {
         try {
           val t = tokensFromPlainText.get(i)
           plainTextTokenWithOffsetss.add(t)
         } catch {
           case e: Exception =>
             e.printStackTrace()
           //stop = true;
         }

         {
           i += 1;
           i - 1
         }
       }
       if ((xmlTokenWithOffsetss.size == 1) && (plainTextTokenWithOffsetss.size == 1)) handleSimpleSubstitution(xmlTokenWithOffsetss.toList, plainTextTokenWithOffsetss.toList, 0, 0)
       else if (!handleSequence(xmlTokenWithOffsetss.toList, plainTextTokenWithOffsetss.toList)) {
         var i = 0
         while ( {
           i < xmlTokenWithOffsetss.size
         }) {
           // val e = xmlTokenWithOffsetss.get(i).element
           //e.setAttribute("misAlignment", "true:" + xmlTokenWithOffsetss.size + "/" + plainTextTokenWithOffsetss.size)

           {
             i += 1;
             i - 1
           }
         }
       }
     }
     */
  protected def transferAnnotationsToElement(t1: T): Unit = {

  }

  private def containsText(tokens: List[TokenWithOffsets]) = {
    for (t <- tokens) {
      if (t.text.length > 0)  true
    }
    false
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

object testje
{
  case class ElementWithPosition(e: Elem, p: Int)
  {
    override def toString() = s"[$p: ${e.text}/${e \\ "@tpart2b"}/(${e \\ "@__code"}${e \\ "@pos"}) ${e \\ "@id"}]"


  }

  object comp extends Comparator[ElementWithPosition]
  {
    override def compare(t: ElementWithPosition, t1: ElementWithPosition):Int = t.e.text.compareToIgnoreCase(t1.e.text)
  }

  def main(args: Array[String]) = {
     val d0 = XML.load(args(0)) // vu
     val d1 = XML.load(args(1)) // hvh

     val seq0 = (d0 \\ "w").zipWithIndex.map({case (e,p) => ElementWithPosition(e.asInstanceOf[Elem],p)}).toList
     val seq1 = (d1 \\ "w").filter(w => !(w \\ "@pos").toString.contains("Punc")).zipWithIndex.map({case (e,p) => ElementWithPosition(e.asInstanceOf[Elem],p)}).toList

    val a = new AlignmentGeneric[ElementWithPosition](comp)
    val (diffs,sims) = a.findDiffsAndSimilarities(seq0, seq1)

    //System.exit(1)

    val hvhgrouping = (d1 \\ "wgroup").map(g => (g \ "@id").toString -> (g \\ "w").map(w => (w \ "@id").toString))
    val platter = hvhgrouping.flatMap({case (x,l) => l.map((x,_))})
    val w2g = platter.groupBy(_._2).mapValues({_.head._1})

    val correspondences:List[List[(List[String], List[String])]] = diffs.map(d =>
    {
      val s0 = seq1.slice(d.getAddedStart, d.getAddedEnd+1)
      val s1 = seq0.slice(d.getDeletedStart, d.getDeletedEnd+1)
      Console.err.println(s"""
         |######
         |  ${s0.map(x => x.toString()).mkString( " ")}
         |  ${s1.map(x => x.toString()).mkString( " ")}
         """.stripMargin)
      val s1Ids = s1.map(x => x.e \\ "@id").map(_.toString)
      val s0Ids = s0.map(x => x.e \\ "@id").map(_.toString)
      if (s1.size > 1)
        {
          val grouped = s0Ids.groupBy(x => if (w2g.containsKey(x)) w2g(x) else x).toList
          if (grouped.size == s1Ids.size)
            { val z =s1Ids.zipWithIndex.map({case (id,k) => List(id) -> grouped(k)._2}); Console.err.println(s"Leuk: $z") ; z }
          else  List((s1Ids, s0Ids))
        } else
      List((s1Ids, s0Ids))
    }
    )

    val simcorresps =  sims.map(s => {
      val s0 = seq1.slice(s.s1, s.s1+s.length)
      val s1 = seq0.slice(s.s2, s.s2+s.length)
      (s1.map(x => x.e \\ "@id"), s0.map(x => x.e \\ "@id"))
    })

    val all = correspondences.flatten ++ simcorresps.flatMap({case (l1,l2) => l1.zip(l2)})

    val vu2hvh = all.filter(_._1.size == 1).map({case (x,y) => x.head.toString -> y.map(id => s"#${id}").mkString(" ")}).toMap

    //println(vu2hvh)
    //println(vu2hvh.keySet.size)
    //println(simcorresps)
    // System.exit(0)

    def addHvhLink(w: Elem):Elem = {
      val id = (w \\ "@id").toString
      val hvh = if (vu2hvh.containsKey(id)) vu2hvh(id) else ""
      if (hvh == "") w else  w.copy(attributes =  w.attributes.append(new UnprefixedAttribute("link", hvh, Null)))
    }

    val updatedVU = PostProcessXML.updateElement(d0, _.label == "w", addHvhLink)
    println(updatedVU)
    // println(s"Matched tokens: ${sims.map(s => s.length).sum}")
  }
}
