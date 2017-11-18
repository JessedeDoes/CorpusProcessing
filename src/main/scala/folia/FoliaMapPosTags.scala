package folia
import java.io.FileInputStream
import java.util.zip.GZIPInputStream

import scala.xml._
import posmapping._

/*
ToDo: houd rekening met de sets.
- Pas alleen toe op tags uit een op te geven set
- Pas de setdeclaratie in de header aan
 */

case class FoliaMapPosTags(parseTag: String=>Tag, tagMapping: Tag => Tag) {

  val cachedMapping = Cache(tagMapping)

  def updateElement(e: Elem, condition: Elem=>Boolean, f: Elem => Elem):Elem =
  {
    if (condition(e))
      f(e)
    else
      e.copy(child = e.child.map({
        {
          case e1: Elem => updateElement(e1,condition,f)
          case n:Node => n
        }
      }))
  }

  def mapPosElement(e:Elem):Elem =
  {
    val tag = parseTag((e \ "@class").toString)
    val newTag = cachedMapping(tag)
    val features = newTag.features.map({case Feature(n,v) => <feat class={n} value={v}/>})
    e.copy(child=features,
      attributes = e.attributes
        .remove("class")
        .remove("confidence")
        .append(new UnprefixedAttribute("class", newTag.toString, Null)) )
  }

  def updatePoS(d: Elem):Elem = updateElement(d, e => e.label == "pos", mapPosElement)

  def updatePoS(fileName: String):Unit =
    {
      val in = new FileInputStream(fileName)
      val zin = if (fileName.endsWith("gz")) new GZIPInputStream(in) else in
      println(updatePoS(XML.load(zin)))
    }
}

// ToDo deze manier van cachen is niet threadsafe

case class Cache[A,B](f: A=>B)
{
  val map = scala.collection.mutable.HashMap.empty[A,B]
  def apply(a: A):B = map.getOrElseUpdate(a, f(a))
}

object TestSimplify
{
  def simplify(t: Tag):Tag = t.asInstanceOf[CGNTag].lightTag.asInstanceOf[Tag] // CGNTag(s"${t.pos}()")
  def toUD(t:Tag):Tag = CGNPoSTagging.toUD(t)
  def toLite(t:Tag):Tag = CGNPoSTagging.toLite(t)
  def cgnParse(t: String):Tag = CGNTagset.fromString(t)
  def main(args: Array[String]):Unit = FoliaMapPosTags(cgnParse, toLite).updatePoS(args(0))
}