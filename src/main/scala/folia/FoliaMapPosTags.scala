package folia
import java.io.FileInputStream
import java.util.zip.GZIPInputStream

import scala.xml._
import posmapping._
import utils.PostProcessXML

/*
ToDo: houd rekening met de sets.
- Pas alleen toe op tags uit een op te geven set
- Pas de setdeclaratie in de header aan
 */

case class FoliaMapPosTags(parseTag: String=>Tag, tagMapping: Tag => Tag, targetSet: String = "UDv2.0") {

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

  def mapPosElement(e:Elem):NodeSeq =
  {
    val tag = parseTag((e \ "@class").toString)
    val newTag = cachedMapping(tag)
    val features = newTag.features.map({case Feature(n,v) => <feat class={n} value={v}/>})
    val sourceSet = (e \ "@set").text

    val translated = e.copy(child=features,
      attributes = e.attributes
        .remove("class")
        .remove("confidence")
        .remove("set")
        .append(new UnprefixedAttribute("class", newTag.toString, Null))
        .append(new UnprefixedAttribute("set", sourceSet + "=>" + targetSet, Null)))
    Seq(translated, e)
  }

  def updatePoS(d: Elem):Elem = PostProcessXML.updateElement2(d, e => e.label == "pos", mapPosElement).asInstanceOf[Elem] // nee zo wil ik het niet ...

  def mapPoS(fileName: String): Elem =
  {
    val in = new FileInputStream(fileName)
    val zin = if (fileName.endsWith("gz")) new GZIPInputStream(in) else in
    updatePoS(XML.load(zin)).asInstanceOf[Elem]
  }

  def updatePoS(fileName: String):Unit =
    {
      println(mapPoS(fileName))
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

