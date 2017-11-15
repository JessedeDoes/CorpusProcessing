package sat
import scala.util.Success
import scala.util.matching._

case class Feature(name: String, value: String)

abstract class Tag(tag: String, tagset: TagSet)
{
  val pos:String
  val features:List[Feature]
  def parseFeature(f:String):Feature
  def proposition:Proposition
}

case class TagSet(prefix: String,
                  posTags: List[String] = List.empty,
                  partitions:Map[String,Set[String]] = Map.empty,
                  pos2partitions: Map[String,List[String]] = Map.empty)
{
  def inSubsets(f:String):List[String] = scala.util.Try (
    {
      val z = partitions.filter({ case (s, v) => v.contains(f) })
      z.toList.map(_._1)
    })
  match {
    case Success(l) => l
    case scala.util.Failure(s) => List("failure")
  }
}

object UDTagSet extends TagSet("ud")

object CGNTag
{
  lazy val partitions = Map(
    "buiging"  -> Set("met-e", "met-s", "zonder"),
    "getal-n"  -> Set("mv-n", "zonder-n"),
    "lwtype"   -> Set("bep", "onbep"),
    "conjtype" -> Set("neven", "onder"),
    "ntype"    -> Set("eigen", "soort"),
    "numtype"  -> Set("hoofd", "rang"),
    "getal"    -> Set("ev", "mv", "getal"), // bij substantieven: getal
    "pvagr"    -> Set("met-t", "ev", "mv"), // kan dit???
    "pvtijd"   -> Set("tgw", "verl", "conj", "imp"), // conjunctief een tijd? ja.
    "status"   -> Set("nadr", "red", "vol"),
    "vztype"   -> Set("init", "fin", "versm"),
    "graad"    -> Set("basis", "comp", "dim", "sup"),
    "pdtype"   -> Set("adv-pron", "det", "grad", "pron"), // wat is grad? en doe iets met adv-pron!
    "positie"  -> Set("nom", "postnom", "prenom", "vrij"),
    "genus"    -> Set("fem", "genus", "masc", "onz", "zijd"),
    "naamval"  -> Set("bijz", "dat", "gen", "nomin", "obl", "stan"),
    "persoon"  -> Set("1", "2", "2b", "2v", "3", "3m", "3o", "3p", "3v", "persoon"),
    "npagr"    -> Set("agr", "agr3", "evf", "evmo", "evon", "evz", "mv", "rest", "rest3"),
    "wvorm"    -> Set("inf", "od", "pv", "vd"),
    "vwtype"   -> Set("refl", "aanw", "betr", "bez", "excl", "onbep", "pers", "pr", "recip", "vb", "vrag"),
    "spectype" -> Set("deeleigen", "vreemd", "afk", "afgebr", "symb", "meta"),
    "variatie" -> Set("dial") // officiele naam??
  )

  lazy val pos2partitions = List( // fix volgorde....
    "N"    -> List("ntype", "getal", "graad", "genus", "naamval"),
    "ADJ"  -> List("positie", "graad", "buiging", "getal-n", "naamval"),
    "WW"   -> List("wvorm", "pvtijd", "pvagr", "positie", "buiging", "getal-n"),
    "NUM"  -> List("numtype", "positie", "graad", "getal-n", "naamval"),
    "VNW"  -> List("vwtype", "pdtype", "naamval", "status", "persoon", "getal", "genus", "positie",  "buiging", "npagr", "getal-n", "graad"),
    "LID"  -> List("lwtype", "naamval", "npagr"),
    "VZ"   -> List("vztype"),
    "VG"   -> List("conjtype"),
    "BW"   -> List(),
    "TSW"  -> List(),
    "SPEC" -> List("spectype")
  ).toMap

  lazy val posTags = pos2partitions.keySet.toList

  val CGNTagset = TagSet("cgn", posTags, partitions, pos2partitions)

  lazy val lightFeaturesSet = Set("pos", "positie", "conjtype", "wvorm", "vwtype", "prontype", "pdtype")

  lazy val allTags:List[CGNTag] = scala.io.Source.fromFile("data/cgn.tagset").getLines().toList.sorted.map(CGNTag(_))

  lazy val allLightTags0:List[CGNTag] = allTags.map(_.lightTag).toSet.toList

  lazy val allLightTags = allLightTags0.sortBy(_.toString)

  def checkAllTags:Unit = allTags.foreach(t => {
    val p = t.proposition
    val tLight = t.lightTag
    val pLight = tLight.proposition

    val p1 = scala.util.Try(CGNMapping.mapToTagset(p, CGNMapping.udFeatureSet)).get
    println(s"$t -> $p1")

    val p1Light = scala.util.Try(CGNMapping.mapToTagset(pLight, CGNMapping.udFeatureSet)).get
    println(s"$tLight -> $p1Light")
  })

  def checkLightTags:Unit = allLightTags.foreach(t => {
    val p = t.proposition
    val p1 = scala.util.Try(CGNMapping.mapToTagset(p, CGNMapping.udFeatureSet)).get
    println(s"$t\t$p\t$p1")
  })

  def main(args: Array[String]):Unit = checkLightTags
}

class UDStyleTag(tag: String, tagset: TagSet) extends Tag(tag,tagset)
{
  val Tag = new Regex("^([A-Z]+)\\((.*?)\\)")
  val Tag(pos,feats) = tag

  val featureValues:Array[String] = feats.split("\\s*,\\s*").filter(f => !(f.trim == ""))
  val features:List[Feature] = featureValues.map(parseFeature).toList ++ List(Feature("pos", pos))
  def parseFeature(f: String):Feature = {val c = f.split("\\s*,\\s*"); Feature(c(0), c(1))}

  def proposition:Proposition = And(features.map({case Feature(n,v) => Literal(s"${tagset.prefix}:$n=$v") } ) :_*)
}

class CGNStyleTag(tag: String, tagset: TagSet) extends Tag(tag,tagset)
{
  val Tag = new Regex("^([A-Z]+)\\((.*?)\\)")
  val Tag(pos,feats) = tag

  val featureValues:Array[String] = feats.split("\\s*,\\s*").filter(f => !(f.trim == ""))

  val features:List[Feature] = featureValues.map(parseFeature).toList ++ List(Feature("pos", pos))

  def parseFeature(f: String) = Feature(getFeatureName(f),f)

  def getFeatureName(f: String):String =
  {
    val p: String = this.pos
    val V: List[String] = this.tagset.inSubsets(f)

    val fn = if (V.isEmpty) "UNK"
    else if (V.size == 1) V.head
    else {
      val V1 = V.filter(n => tagset.pos2partitions(p).contains(n))
      if (V1.nonEmpty) V1.head else V.mkString("|")
    }

    fn
  }

  def proposition:Proposition = And(features.map({case Feature(n,v) => Literal(s"${tagset.prefix}:${if (n != "pos") "feat." else ""}$n=$v") } ) :_*)
}

case class CGNTag(tag: String) extends CGNStyleTag(tag, CGNTag.CGNTagset)
{
  import CGNTag._

  val problems:Array[String] = featureValues.filter(f => getFeatureName(f).contains("UNK") || getFeatureName(f)=="")

  if (problems.nonEmpty) Console.err.println(s"PROBLEM FOR FEATURE VALUES  IN ($tag):" + problems.toList)

  lazy val lightFeatures:List[Feature] = features.filter({ case Feature(n,v) => lightFeaturesSet.contains(n)})

  lazy val lightTag = CGNTag(s"$pos(${lightFeatures.filter(f => !(f.name=="pos")).map(_.value).mkString(",")})")

  override def toString:String = tag

  def openSonarLink:String =
  {
    import java.net.URLEncoder

    val pattern = s"""[pos='$pos.${featureValues.mkString(",(.*,)?")}(,.*)?.']"""
    s"http://opensonar.ato.inl.nl/search/expert?patt=${URLEncoder.encode(pattern)}&within=document&view=1&offset=0&number=10#results"
  }
}

