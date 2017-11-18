package folia
import java.io.FileInputStream
import java.util.zip.GZIPInputStream

import scala.xml._

case class FoliaSampler(document: Elem, numWords: Int)
{
   lazy val sentencesShuffled = scala.util.Random.shuffle((document \\ "s").filter(textLength(_) > 10).toList)
   lazy val paragraphsShuffled = scala.util.Random.shuffle((document \\ "p")
     .filter(p => textLength(p) > 30 && textLength(p) < Math.max(numWords / 2, 100)).toList)

   def textLength(n: Node) = (n \\ "t").size

   def addToSample(acc:(Set[Node], Int), s: Node) = if (acc._2 > numWords) acc else (acc._1 + s, acc._2 + s.size)

   lazy val sentenceSample = sentencesShuffled.foldLeft(Set.empty[Node], 0)(addToSample)
   lazy val paragraphSample  = paragraphsShuffled.foldLeft(Set.empty[Node], 0)(addToSample)

   def expandKeepSet(node: Node, filter: Node => Boolean):Set[Node] =
     if (filter(node)) node.descendant_or_self.toSet else
       {
         val below = node.child.flatMap(expandKeepSet(_, filter)).toSet
         if (below.nonEmpty) below ++ Set(node)
         else Set()
       }

   def sample(node:Node, keepSet: Set[Node]):Option[Node] =
   {
     node match {
       case e: Elem if keepSet.contains(e) => Some(e.copy(child = e.child.map(sample(_,keepSet)).filter(_.isDefined).map(_.get) ))
       case n: Node => if (keepSet.contains(n)) Some(n) else None
     }
   }

   def sample():Option[Node] =
   {
     paragraphSample._1.foreach(s => Console.err.println((s \\ "t").map(_.text).mkString(" ")))
     val keepjes = expandKeepSet(document, n => paragraphSample._1.contains(n) || n.label == "metadata")
     sample(document, keepjes)
   }
}

object FoliaSampler
{
  def main(args: Array[String]):Unit  =
  {
    val folia = XML.load(new GZIPInputStream(new FileInputStream(args(0))))
    val sample = FoliaSampler(folia, 300).sample()
    if (sample.isDefined) {
      println(sample.get)
    }
  }
}