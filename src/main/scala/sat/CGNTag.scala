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
    "getal" -> Set("ev", "mv", "getal"),
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
  );

  def inSubsets(f:String):List[String] = subsets.filter( {case (s,v) => v.contains(f)} ).toList.map(_._1)

  lazy val allTags = scala.io.Source.fromFile("data/cgn.tagset").getLines().map(CGNTag(_))

  // println(CGNTag("LID(onbep)").proposition)

  allTags.foreach(t => {
    val p = t.proposition
    // println(p)
    val p1 = scala.util.Try(CGNMapping.mapToTagset(p, CGNMapping.udFeatureSet)).get
    println(s"$t -> $p1")
  })
}

case class Feature(name: String, Value: String)

case class CGNTag(tag: String)
{
  import CGNTag._
  val Tag = new Regex("^([A-Z]+)\\((.*?)\\)")
  val Tag(pos,feats) = tag
  val prefeats:List[Feature] = feats.split("\\s*,\\s*").map(s => Feature( inSubsets(s).mkString("|"),s)).toList ++ List(Feature("pos", pos))
  val features = prefeats.map(
    f => f match {
      case Feature("lwtype|vwtype","onbep") if (pos=="LID") => Feature("lwtype", "onbep")
      case Feature("lwtype|vwtype","onbep") if (pos=="VNW") => Feature("vwtype", "onbep")
      case Feature("pvagr|getal",a ) if (pos=="WW") => Feature("pvagr", a)
      case Feature("pvagr|getal",a ) if (pos !="WW") => Feature("getal", a)
      case _ => f
    })
  lazy val proposition:Proposition = And(features.map({case Feature(n,v) => Literal(s"cgn:${if (n != "pos") "feat." else ""}$n=$v") } ) :_*)
}

