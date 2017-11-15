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

abstract case class TagSet(prefix: String,
                           val posTags: List[String] = List.empty,
                  partitions:Map[String,Set[String]] = Map.empty,
                  pos2partitions: Map[String,List[String]] = Map.empty)
{
  def inSubsets(f:String):List[String] = partitions.filter({ case (s, v) => v.contains(f) }).toList.map(_._1)
  def fromProposition(p:Proposition):Tag
  def fromString(t: String):Tag
}

object UDTagSet extends TagSet("ud")
{
  def fromProposition(p:Proposition):Tag =
  {
    val vars = p.varsIn
    val pos = vars.find(v => v.startsWith(s"${this.prefix}:pos=")).getOrElse("UNK").replaceAll(".*=", "")
    val features = vars.map(v => v.replaceAll(s"^${this.prefix}:", "")).mkString(",")
    new UDStyleTag(s"$pos($features)", this)
  }
  override def fromString(t: String): Tag = new UDStyleTag(t, this)
}

object CGNPoSTagging
{
  //Console.err.println("eek!!")
  def partitions = Map(
    "buiging"  -> Set("met-e", "met-s", "zonder"),
    "getal-n"  -> Set("mv-n", "zonder-n"),
    "lwtype"   -> Set("bep", "onbep"),
    "conjtype" -> Set("neven", "onder"),
    "ntype"    -> Set("eigen", "soort"),
    "numtype"  -> Set("hoofd", "rang"),
    "getal"    -> Set("ev", "mv", "getal"),
    "pvagr"    -> Set("met-t", "ev", "mv"),
    "pvtijd"   -> Set("tgw", "verl", "conj", "imp"),
    "status"   -> Set("nadr", "red", "vol"),
    "vztype"   -> Set("init", "fin", "versm"),
    "graad"    -> Set("basis", "comp", "dim", "sup"),
    "pdtype"   -> Set("adv-pron", "det", "grad", "pron"),
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

  def pos2partitions = List( // fix volgorde....
    "N"    -> List("ntype", "getal", "graad", "genus", "naamval"),
    "ADJ"  -> List("positie", "graad", "buiging", "getal-n", "naamval"),
    "WW"   -> List("wvorm", "pvtijd", "pvagr", "positie", "buiging", "getal-n"),
    "NUM"  -> List("numtype", "positie", "graad", "getal-n", "naamval"),
    "VNW"  -> List("vwtype", "pdtype", "naamval", "status", "persoon",
                      "getal", "genus", "positie",  "buiging", "npagr", "getal-n", "graad"),
    "LID"  -> List("lwtype", "naamval", "npagr"),
    "VZ"   -> List("vztype"),
    "VG"   -> List("conjtype"),
    "BW"   -> List(),
    "TSW"  -> List(),
    "SPEC" -> List("spectype")
  ).toMap

  def posTags = pos2partitions.keySet.toList

  lazy val lightFeaturesSet = Set("pos", "positie", "conjtype", "wvorm", "vwtype", "prontype", "pdtype")

  lazy val allTags:List[CGNTag] = scala.io.Source.fromFile("data/cgn.tagset").getLines().toList.sorted.map(CGNTag(_))

  lazy val allLightTags0:List[CGNTag] = allTags.map(_.lightTag).toSet.toList

  lazy val allLightTags = allLightTags0.sortBy(_.toString)

  val mappingToUD = PropositionalTagsetMapping("data/mapping.sorted", ()=>CGNTagset, ()=>UDTagSet)
  val mappingToLite = PropositionalTagsetMapping("data/cgn2lite.prop", ()=>CGNTagset, ()=>CGNLiteTagset)

  def toUD(t: Tag):Tag = mappingToUD.mapTag(t)
  def toLite(t: Tag):Tag = mappingToLite.mapTag(t)

  def checkAllTags:Unit = allTags.foreach(t => {
    val p = t.proposition
    val tLight = t.lightTag
    val pLight = tLight.proposition

    val p1 = scala.util.Try(mappingToUD.mapToTagset(p)).get
    println(s"$t -> $p1")

    val p1Light = scala.util.Try(mappingToUD.mapToTagset(pLight)).get
    println(s"$tLight -> $p1Light")
  })

  def checkLightTags:Unit = allLightTags.foreach(t => {
    val p = t.proposition
    val p1 = scala.util.Try(mappingToUD.mapToTagset(p)).get
    println(s"$t\t$p\t$p1")
  })

  def main(args: Array[String]):Unit = checkLightTags
  //Console.err.println("peek!!")
}

object CGNTagset extends TagSet("cgn", CGNPoSTagging.posTags, CGNPoSTagging.partitions, CGNPoSTagging.pos2partitions)
{
  //Console.err.println(s"blurk $posTags")

  def fromProposition(p:Proposition):Tag =
  {
    val vars = p.varsIn
    Console.err.println(p)
    val pos = vars.find(v => v.startsWith(s"${this.prefix}:pos=")).get.replaceAll(".*=", "")
    val features = vars.filter(f => !f.startsWith(s"${this.prefix}:pos=")).map(v => v.replaceAll(s"^.*=", "")).mkString(",")
    new CGNStyleTag(s"$pos($features)", this)
  }

  override def fromString(t: String): Tag = CGNTag(t)
}

object CGNLite
{
  def partitions = Map(

    "conjtype" -> Set("neven", "onder"),
    "ntype"    -> Set("eigen", "soort"),
    "vztype"   -> Set("init", "fin", "versm"),
    "graad"    -> Set("basis", "comp", "dim", "sup"),
    "pdtype"   -> Set("adv-pron", "det", "grad", "pron"), // wat is grad? en doe iets met adv-pron!
    "positie"  -> Set("nom", "postnom", "prenom", "vrij"),
    "wvorm"    -> Set("inf", "od", "pv", "vd"),
    "vwtype"   -> Set("refl", "aanw", "betr", "bez", "excl", "onbep", "pers", "recip", "vb"),
    "spectype" -> Set("deeleigen", "vreemd", "afk", "afgebr", "symb", "meta"),
    "variatie" -> Set("dial") // officiele naam??
  )

  def pos2partitions = List( // fix volgorde....
    "N"    -> List("ntype"),
    "ADJ"  -> List("positie", "graad"),
    "WW"   -> List("wvorm",  "positie"),
    "NUM"  -> List("numtype", "positie"),
    "VNW"  -> List("vwtype", "pdtype",  "positie"),
    "VZ"   -> List("vztype"),
    "VG"   -> List("conjtype"),
    "BW"   -> List(),
    "TSW"  -> List(),
    "SPEC" -> List("spectype")
  ).toMap

  def posTags = pos2partitions.keySet.toList
}

object CGNLiteTagset extends TagSet("cgnl", CGNLite.posTags, CGNLite.partitions, CGNLite.pos2partitions)
{
  def fromProposition(p:Proposition):Tag =
  {
    val vars = p.varsIn

    val pos = vars.find(v => v.startsWith(s"${this.prefix}:pos=")).getOrElse("UNK").replaceAll(".*=", "")
    val features = vars.filter(f => !f.startsWith(s"${this.prefix}:pos=")).map(v => v.replaceAll(s"^.*=", "")).mkString(",")

    var x = new CGNStyleTag(s"$pos($features)", this)
    Console.err.println(s"Proposition to tag: $p->$x")
    x
  }
  override def fromString(t: String): Tag = CGNLiteTag(t)
}

class UDStyleTag(tag: String, tagset: TagSet) extends Tag(tag,tagset)
{
  //Console.err.println(s"Yes: $tag...")
  val Tag = new Regex("^([A-Z\\|]+)\\((.*?)\\)")
  val Tag(pos,feats) = tag

  val featureValues:Array[String] = feats.split("\\s*,\\s*").filter(f => !(f.trim == ""))
  val features:List[Feature] = featureValues.map(parseFeature).toList ++ List(Feature("pos", pos))
  def parseFeature(f: String):Feature = {val c = f.split("\\s*=\\s*"); Feature(c(0), c(1))}

  def proposition:Proposition = And(features.map({case Feature(n,v) => Literal(s"${tagset.prefix}:$n=$v") } ) :_*)

  override def toString:String = tag
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

  override def toString:String = tag

  def proposition:Proposition = And(features.map({case Feature(n,v) => Literal(s"${tagset.prefix}:${if (n != "pos") "feat." else ""}$n=$v") } ) :_*)
}

case class CGNLiteTag(tag: String) extends CGNStyleTag(tag, CGNLiteTagset)

case class CGNTag(tag: String) extends CGNStyleTag(tag, CGNTagset)
{
  import CGNPoSTagging._

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
