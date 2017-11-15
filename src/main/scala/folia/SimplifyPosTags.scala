package folia
import java.io.FileInputStream
import java.util.zip.GZIPInputStream

import scala.xml._
import sat._

case class SimplifyPosTags(tagMapping: sat.CGNTag => sat.CGNTag) {

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

  def editPosElement(e:Elem):Elem =
  {
    val tag = CGNTag((e \ "@class").toString)
    val newTag = tagMapping(tag)
    val features = newTag.features.map({case Feature(n,v) => <feat class={n} value={v}/>})
    e.copy(child=features, attributes = e.attributes.remove("class").append(new UnprefixedAttribute("class", newTag.toString, Null)) )
  }

  def updatePoS(d: Elem):Elem = updateDocument(d, e => e.label == "pos", editPosElement)

  def updatePoS(fileName: String):Unit =
    {
      val in = new FileInputStream(fileName)
      val zin = if (fileName.endsWith("gz")) new GZIPInputStream(in) else in
      println(updatePoS(XML.load(zin)))
    }
}

object TestSimplify
{
  def simplify(t: CGNTag) = CGNTag(s"${t.pos}()")

  def main(args: Array[String]) = SimplifyPosTags(simplify).updatePoS(args(0))
}