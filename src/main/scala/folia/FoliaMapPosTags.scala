package folia
import java.io.FileInputStream
import java.util.zip.GZIPInputStream

import scala.xml._
import sat._

case class FoliaMapPosTags(tagMapping: sat.CGNTag => sat.CGNTag) {

  val cachedMapping = Cache(tagMapping)

  def updateDocument(e: Elem, condition: Elem=>Boolean, f: Elem => Elem):Elem =
  {
    if (condition(e))
      f(e)
    else
      e.copy(child = e.child.map({
        {
          case e1: Elem => updateDocument(e1,condition,f)
          case n:Node => n
        }
      }))
  }

  def mapPosElement(e:Elem):Elem =
  {
    val tag = CGNTag((e \ "@class").toString)
    val newTag = cachedMapping(tag)
    val features = newTag.features.map({case Feature(n,v) => <feat class={n} value={v}/>})
    e.copy(child=features,
      attributes = e.attributes.remove("class").remove("confidence").append(new UnprefixedAttribute("class", newTag.toString, Null)) )
  }

  def updatePoS(d: Elem):Elem = updateDocument(d, e => e.label == "pos", mapPosElement)

  def updatePoS(fileName: String):Unit =
    {
      val in = new FileInputStream(fileName)
      val zin = if (fileName.endsWith("gz")) new GZIPInputStream(in) else in
      println(updatePoS(XML.load(zin)))
    }
}

case class Cache[A,B](f: A=>B)
{
  val map = scala.collection.mutable.HashMap.empty[A,B]
  def apply(a: A) = map.getOrElseUpdate(a, f(a))
}

object TestSimplify
{
  def simplify(t: CGNTag) = t.lightTag // CGNTag(s"${t.pos}()")

  def main(args: Array[String]) = FoliaMapPosTags(simplify).updatePoS(args(0))
}