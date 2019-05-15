package posmapping
import java.io.File

import propositional.{And, Literal, Proposition}

import scala.util.matching.Regex
import scala.xml._

class CHNStyleTag(tag: String, tagset: TagSet = null) extends Tag(tag,tagset)
{
  val Tag = new Regex("^([A-Z]+)\\((.*?)\\)")



  val tagToParse = if (Tag.findFirstIn(tag).isEmpty) "RES(type=other)" else tag.replaceAll(",other", ",type=other")

  if (Tag.findFirstIn(tagToParse).isEmpty)
  {
    Console.err.println(s"PAS OP Mislukte tag: $tagToParse")
  }

  val Tag(pos,feats) = tagToParse

  val featureValues:Array[String] = feats.split("\\s*,\\s*").filter(f => !(f.trim == ""))

  val features:List[Feature] = featureValues.map(parseFeature).toList ++ List(Feature("pos", pos))

  import scala.util.{Try, Success, Failure}

  def parseFeature(f: String) = {
    val x = f.split("=")
    Try(Feature(x(0),x(1))) match
      {
      case Success(f) => f
      case _ => Feature("kwip", f)
    }
  }
  def proposition:Proposition = And(features.map({case Feature(n,v) => Literal(s"${tagset.prefix}:${if (n != "pos") "feat." else ""}$n=$v") } ) :_*)
}

object CHNStyleTags {
  def tagsetFromSetOfStrings(prefix: String, s: Set[String]) =
  {
    val tags = s.map(s => new CHNStyleTag(s))
    val posTags = tags.map(_.pos).toList
    val partitions:Map[String,Set[String]] = tags.flatMap(_.features).groupBy(_.name).mapValues(x => x.map(f => f.value))
    val pos2partitions:Map[String, List[String]] = tags.groupBy(_.pos).mapValues(l => l.flatMap(p => p.features.map(f => f.name))).mapValues(s => s.toList)

    TagSet(prefix, posTags, partitions)
  }

  def parseTag(s: String): Tag = new CHNStyleTag(s)

  val gysTagset = TagSet.fromXML("data/CRM/gys.tagset.xml")
}

object distinctTagsFromGysseling
{
  def main(args: Array[String]): Unit = {
    val dir = args(0)
    val files:Set[File] = new File(dir).listFiles().toSet
    val allDistinctTags = files.flatMap(
      f => {
        val doc = XML.loadFile(f)

        val combiposjes = (doc \\ "w").map(x => (x \ "@function").text).toSet
        val posjes = combiposjes.flatMap(p => p.split("\\+").toSet)
        println(posjes)
        posjes
      }
    )
    val tagset = CHNStyleTags.tagsetFromSetOfStrings("gys", allDistinctTags)
    println(tagset.toXML)
  }
}
