package sat
import scala.util.matching._

object CGNTag extends App
{
  val subsets = Map(
    "buiging" -> Set("met-e", "met-s", "zonder"),
    "getal-n" -> Set("mv-n", "zonder-n"),
    "lwtype" -> Set("bep", "onbep"),
    "conjtype" -> Set("neven", "onder"),
    "ntype" -> Set("eigen", "soort"),
    "numtype" -> Set("hoofd", "rang"),
    "getal" -> Set("ev", "mv", "getal"), // bij substantieven: getal
    "pvagr" -> Set("met-t", "ev", "mv"), // kan dit???
    "pvtijd" -> Set("tgw", "verl", "conj", "imp"), // conjunctief een tijd? ja.
    "status" -> Set("nadr", "red", "vol"),
    "vztype" -> Set("init", "fin", "versm"),
    "graad" -> Set("basis", "comp", "dim", "sup"),
    "pdtype" -> Set("adv-pron", "det", "grad", "pron"), // wat is grad? en doe iets met adv-pron!
    "positie" -> Set("nom", "postnom", "prenom", "vrij"),
    "genus" -> Set("fem", "genus", "masc", "onz", "zijd"),
    "naamval" -> Set("bijz", "dat", "gen", "nomin", "obl", "stan"),
    "persoon" -> Set("1", "2", "2b", "2v", "3", "3m", "3o", "3p", "3v", "persoon"),
    "npagr" -> Set("agr", "agr3", "evf", "evmo", "evon", "evz", "mv", "rest", "rest3"),
    "wvorm" -> Set("inf", "od", "pv", "vd"),
    "vwtype" -> Set("refl", "aanw", "betr", "bez", "excl", "onbep", "pers", "pr", "recip", "vb", "vrag")
  ) // onbep, ev, mv komen nog in meerdere setjes voor...

  val pos2subsets = List(
    "N" -> List("ntype", "getal", "graad", "genus", "naamval"),
    "ADJ" -> List("positie", "graad", "buiging", "getal-n", "naamval"),
    "WW" -> List("wvorm", "pvtijd", "pvagr", "positie", "buiging", "getal-n"),
    "NUM" -> List("numtype", "positie", "graad", "getal-n", "naamval"),
    "VNW" -> List("vwtype", "pdtype", "naamval", "status", "persoon", "getal", "genus", "positie",  "buiging", "npagr", "getal-n", "graad"),
    "LID" -> List("lwtype", "naamval", "npagr"),
    "VZ" -> List("vztype"),
    "VG" -> List("conjtype"),
    "BW" -> List(),
    "TSW" -> List(),
    "SPEC" -> List("spectype")
  ).toMap

  val lightFeaturesSet = Set("pos", "positie", "conjtype", "wvorm", "vwtype", "prontype", "pdtype")

  def inSubsets(f:String):List[String] = subsets.filter( {case (s,v) => v.contains(f)} ).toList.map(_._1)

  lazy val allTags:List[CGNTag] = scala.io.Source.fromFile("data/cgn.tagset").getLines().toList.sorted.map(CGNTag(_))

  lazy val allLightTags0:List[CGNTag] = allTags.map(_.lightTag).toSet.toList

  lazy val allLightTags = allLightTags0.sortBy(_.toString)

  // println(CGNTag("LID(onbep)").proposition)

  def checkAllTags = allTags.foreach(t => {
    val p = t.proposition
    val tLight = t.lightTag
    val pLight = tLight.proposition
    // println(p)
    val p1 = scala.util.Try(CGNMapping.mapToTagset(p, CGNMapping.udFeatureSet)).get
    println(s"$t -> $p1")

    val p1Light = scala.util.Try(CGNMapping.mapToTagset(pLight, CGNMapping.udFeatureSet)).get
    println(s"$tLight }-> $p1Light")
  })

  def checkLightTags = allLightTags.foreach(t => {
  val p = t.proposition
  val p1 = scala.util.Try(CGNMapping.mapToTagset(p, CGNMapping.udFeatureSet)).get
  println(s"$t ($p) -> $p1")
  })

  checkLightTags
}

case class Feature(name: String, value: String)

case class CGNTag(tag: String)
{
  import CGNTag._
  val Tag = new Regex("^([A-Z]+)\\((.*?)\\)")
  val Tag(pos,feats) = tag

  val featureValues = feats.split("\\s*,\\s*").filter(f => !(f.trim == ""))

  def getFeatureName(f: String):String =
    {
      val p:String = this.pos
      val V:List[String] = inSubsets(f)
      if (V.size == 0) "UNK"
      else  if (V.size ==1) V.head
      else
         {
           val V1 = V.filter(n => pos2subsets(p).contains(n))
           if (V1.nonEmpty) V1.head else V.mkString("|")
         }
    }

  val problems = featureValues.filter(f => getFeatureName(f).contains("UNK") || getFeatureName(f)=="")

  if (problems.nonEmpty) Console.err.println(s"PROBLEM FOR FEATURE VALUES  IN ($tag):" + problems.toList)
  val prefeats:List[Feature] = featureValues.map(s => Feature(getFeatureName(s),s)).toList ++ List(Feature("pos", pos))

  val features = prefeats.map(
    f => f match {
      case Feature("lwtype|vwtype","onbep") if (pos=="LID") => Feature("lwtype", "onbep")
      case Feature("lwtype|vwtype","onbep") if (pos=="VNW") => Feature("vwtype", "onbep")
      case Feature("pvagr|getal",a ) if (pos=="WW") => Feature("pvagr", a)
      case Feature("pvagr|getal",a ) if (pos !="WW") => Feature("getal", a)
      case _ => f
    })

  lazy val proposition:Proposition = And(features.map({case Feature(n,v) => Literal(s"cgn:${if (n != "pos") "feat." else ""}$n=$v") } ) :_*)

  lazy val lightFeatures = features.filter({ case Feature(n,v) => lightFeaturesSet.contains(n)})
  lazy val lightTag = CGNTag(s"$pos(${lightFeatures.filter(f => !(f.name=="pos")).map(_.value).mkString(",")})")
}

