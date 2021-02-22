package folia
import java.io.FileInputStream
import java.util.zip.GZIPInputStream

import posmapping.{CGNPoSTagging, CGNTagset}

import scala.xml._

case class Chain(steps: List[Node => Option[Node]])
{
  def step(n: Option[Node], f: Node=> Option[Node]):Option[Node] = n.flatMap(f)

  def run(n: Node) = steps.foldLeft(Some(n).asInstanceOf[Option[Node]])((n,f) => n.flatMap(f))
}

object PrepareSample {

  def sample(n:Node):Option[Node] = FoliaSampler(n,5000).sample()
  def simplify(n:Node):Option[Node] = Some(FoliaMapPosTags(CGNTagset.fromString, CGNPoSTagging.toLite).updatePoS(n.asInstanceOf[Elem]))
  def toTEI(n:Node):Option[Node] = Some(NederlabToCobalt.convert(n))

  def sampleFromExample(n: Node, f: String):Option[Node] =
  {
    val reference = XML.load(f)
    val idz = (reference \\ "w").map(floep.getId).toSet
    FoliaSampler(n, 5000, idz).sample()
  }

  def prepareSample(doc: Node, f: Option[String] = None):Option[Node] =
  {
    Chain(List(if (f.isDefined) x => sampleFromExample(x,f.get) else sample, simplify, toTEI)).run(doc)
  }

  def main(args: Array[String]) =
  {
    val folia = XML.load(new GZIPInputStream(new FileInputStream(args(0))))
    val sample = prepareSample(folia, if (args.size > 1) Some(args(1)) else None)
    if (sample.isDefined) println(sample.get)
  }
}
