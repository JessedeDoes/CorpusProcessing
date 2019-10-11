package posmapping
import java.io.{File, PrintWriter}

import posmapping.distinctTagsFromONW.dir
import propositional.{And, Literal, Proposition}

import scala.util.matching.Regex
import scala.xml._

class CHNStyleTag(tag: String, tagset: TagSet = null) extends Tag(tag,tagset)
{
  val Tag = new Regex("^([A-Z]+-?[A-Z]*)\\((.*?)\\)")


  val patchedTag = if (!tag.endsWith(")")) s"$tag()" else tag


  val tagToParse = if (Tag.findFirstIn(patchedTag).isEmpty) "RES(type=other)" else patchedTag.replaceAll(",other", ",type=other")

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
      case Success(f) if f.value == "auxiliary" => f.copy(value="aux/cop") // HM.....
      case Success(f) if f.value == "indefinite" => f.copy(value="indef") // HM.....
      case Success(f) => f
      case _ => Feature("kwip", f)
    }
  }
  def proposition:Proposition = And(features.map({case Feature(n,v) => Literal(s"${tagset.prefix}:${if (n != "pos") "feat." else ""}$n=$v") } ) :_*)
}

object CHNStyleTags {

  def tagsetFromSetOfStrings(prefix: String, s: Set[String]) =
  {
    val tags = s.map(s => CHNStyleTags.parseTag(s))
    TagSet.tagsetFromSetOfTags(prefix,tags)
  }

  def parseTag(s: String): Tag = new CHNStyleTag(s)

  object parser extends TagParser {
    def parseTag(ts: TagSet, s: String)  = CHNStyleTags.parseTag(s)
  }

  val gysTagset = TagSet.fromXML("data/CRM/gys.tagset.xml")

  //val onwTagset = TagSet.fromXML("data/ONW/onw.tagset.xml").copy(parser=CHNStyleTags.parser)
}

object distinctTagsFromGysseling
{
  def main(args: Array[String]): Unit = {
    val dir = args(0)

    val attribute = "pos"
    tagsetFromCorpusFiles(dir, attribute)
  }


  def tagsetFromCorpusFiles(dirName: String, attribute: String, separator: String = "[+|]") = {
    val dir = new File(dirName)
    val files: Set[File] = if (dir.isDirectory) dir.listFiles().toSet else Set(dir)
    val allDistinctTags: Set[String] = files.flatMap(
      f => {
        scala.util.Try(
          {
            val doc = XML.loadFile(f)

            val combiposjes = (doc \\ "w").map(x => (x \ s"@$attribute").text).toSet
            val posjes = combiposjes.flatMap(p => p.split(separator).toSet)
            //println(posjes)
            posjes
          }) match {
          case scala.util.Success(x) => x
          case _ => Set[String]()
        }
      }
    )
    val tagset = CHNStyleTags.tagsetFromSetOfStrings("chnpos", allDistinctTags)
    val xmlWriter = new PrintWriter("/tmp/tagset.xml")
    xmlWriter.println(TagSet.pretty.format(tagset.toXML))
    xmlWriter.close()
    val jsonWriter = new PrintWriter("/tmp/tagset.json")
    jsonWriter.println(tagset.asJSON)
    jsonWriter.close()

    val blfWriter = new PrintWriter("/tmp/tagset.blf")
    blfWriter.println(tagset.forBlacklab)
    blfWriter.close()
  }
}


object distinctTagsFromONW {
  val dir = "/home/jesse/workspace/data-historische-corpora/ONW/ONW-processed-metadata"

  def main(args: Array[String]): Unit = {
    distinctTagsFromGysseling.tagsetFromCorpusFiles(dir, "msd", "[+]")
  }
}

object distinctTagsFromBaB
{
  val dir = brievenalsbuit.Settings.output.getCanonicalPath

  def main(args: Array[String]): Unit = {
    distinctTagsFromGysseling.tagsetFromCorpusFiles(dir, "pos")
  }
}

object distinctTagsFromMolex
{
  val file = "data/Molex/fakeDocument.xml"

  def main(args: Array[String]): Unit = {
    distinctTagsFromGysseling.tagsetFromCorpusFiles(file, "pos")
  }
}
