package folia
import java.io.FileInputStream
import java.util.zip.GZIPInputStream

import folia.FoliaToRudimentaryTEI.nonModernized

import scala.xml._

/*
Probleem: sample een beetje "normale" tekst.
Geen foreign
Geen tabellen, inhoudsopgaven
Niet te veel afkortingen etc...

Taalmodel van complete tekst maken en daarmee scoren (Te veel gedoe, geen zin...)
Beter gewoon iets meer sampelen en zooi wegsmijten

Workflow:

Sample
Simplify tagset
Convert back to TEI


Voor een paar bestanden werkt sample per alinea niet (engelsman, ams)
 */
package object floep
{
  def getId(n: Node):String = n.attributes.filter(a => a.prefixedKey.endsWith(":id") ||
    a.key.equals("id")).map(a => a.value.toString).head
}
case class FoliaSampler(document: Node, numWords: Int, echoMe: Set[String] = Set())
{
   lazy val sentencesShuffled:List[Node] = scala.util.Random.shuffle((document \\ "s").filter(textLength(_) > 10).toList)
   lazy val paragraphsShuffled:List[Node] = scala.util.Random.shuffle(((document \\ "p") ++ (document \\ "event"))
     .filter(p => textLength(p) > 30 && textLength(p) < Math.max(numWords / 2, 300) && averageWordLength(p) > 3).toList)

   Console.err.println(s"Paragraphs filtered: ${paragraphsShuffled.size}")

  def getId(n: Node):String = n.attributes.filter(a => a.prefixedKey.endsWith(":id") ||
    a.key.equals("id")).map(a => a.value.toString).head

   def textLength(n: Node):Int = (n \\ "w" \\  "t").count(nonModernized)

   def averageWordLength(n: Node):Double = {
     val words = (n \\ "w" \\  "t").filter(nonModernized).map(_.text)
     words.map(_.length).sum / words.size.toDouble
   }

   def addToSample(acc:(Set[Node], Int), s: Node):(Set[Node], Int) = if (acc._2 > numWords) acc else (acc._1 + s, acc._2 + textLength(s))

   lazy val sentenceSample:(Set[Node], Int) = sentencesShuffled.foldLeft(Set.empty[Node], 0)(addToSample)
   lazy val paragraphSample:(Set[Node], Int)  = paragraphsShuffled.foldLeft(Set.empty[Node], 0)(addToSample)
   lazy val paragraphEchoSample:(Set[Node], Int) = ((document \\ "p").filter(
     p => (p \\ "w").map(getId).exists(id => echoMe.contains(id))
   ).toSet, echoMe.size)

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
     val sampie = if (echoMe.nonEmpty) paragraphEchoSample else paragraphSample
     val samp = sampie._1
     val size = sampie._2
     Console.err.println(s"Size: $size")
     samp.foreach(s => Console.err.println((s \\ "w" \\ "t").filter(nonModernized).map(_.text).mkString(" ")))
     val keepjes = expandKeepSet(document, n => sampie._1.contains(n) || n.label == "metadata")
     Console.err.println(s"keepjes: ${keepjes.size}")

     sample(document, keepjes)
   }


}

object FoliaSampler
{
  def main(args: Array[String]):Unit  =
  {
    val folia = XML.load(new GZIPInputStream(new FileInputStream(args(0))))
    val sample = FoliaSampler(folia, 5000).sample()
    if (sample.isDefined) {
      println(sample.get)
    }
  }
}

object FoliaEchoSampler
{
  def main(args: Array[String]):Unit  =
  {
    val folia = XML.load(new GZIPInputStream(new FileInputStream(args(0))))
    val reference = XML.load(args(1))
    val idz = (reference \\ "w").map(floep.getId).toSet
    val sample = FoliaSampler(folia, 5000, idz).sample()
    if (sample.isDefined) {
      println(sample.get)
    }
  }
}