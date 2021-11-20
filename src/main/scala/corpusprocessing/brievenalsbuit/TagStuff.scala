package corpusprocessing.brievenalsbuit

import corpusprocessing.brievenalsbuit.Settings.multivalSepSplit
import corpusprocessing.brievenalsbuit.TagStuff.tagMapCHNStyle
import corpusprocessing.brievenalsbuit.TagStuff.tagMapTDNStyle
import corpusprocessing.brievenalsbuit.TagStuff.tagMapUDStyle
import posmapping.CHNStyleTags

import scala.xml.{Elem, NodeSeq, Null, UnprefixedAttribute}
import scala.util.{Try,Success,Failure}

/**
  * Beetje ingewikkelde poging om met annotaties om te gaan
  */
trait Annotation {
  def featureStructure() : NodeSeq
  def valid(): Boolean = true

  def map(f: LemPos => LemPos): Annotation = this match {
    case x: LemPos => f(x)
    case Alt(s) => Alt(s.map(x => x.map(f)))
    case Clitic(s) => Clitic(s.map(x => x.map(f)))
  }

  def mapTag(f: String => String): Annotation = map({case LemPos(l,t) => LemPos(l, f(t))})

  def toCHN: Annotation = better().mapTag(p => tagMapTDNStyle.getOrElse(p, s"RES(type=uncl)"))
  def toUD: Annotation = better().mapTag(p => tagMapUDStyle.getOrElse(p, s"X"))
  def better(): Annotation = this.map(_.betterAnnotation())

  def toStrings: LemPos
}

case class LemPos(lemma: String, pos: String)  extends Annotation {
  def featureStructure(): Elem =
  {
    val tag = CHNStyleTags.parseTag(pos)
    Console.err.println(s"$pos --> ${tag.features}")
    val features = CHNStyleTags.gysTagset.asTEIFeatureStructure(tag)
    val lem = <f name="lemma">{lemma}</f>
    features.copy(child = features.child ++ lem)
  }
  lazy val betterPoS: String = pos.replaceAll("[?\\]]","")
  def betterAnnotation(): LemPos = this.copy(pos = this.betterPoS)
  override def valid(): Boolean = betterPoS.matches(".*[A-Z].*")

  def toStrings: LemPos = this
}

case class Alt(x: Seq[Annotation]) extends Annotation
{
  def featureStructure(): NodeSeq =
  {
    val percent = (1 / x.size.toDouble) * 100
    x.flatMap(_.featureStructure()).map(f => f.asInstanceOf[Elem]).zipWithIndex.map(
      { case (e,n) =>
        e.copy(attributes=e.attributes.append(new UnprefixedAttribute("cert", percent.toString , Null)))
      }
    )
  }

  def toStrings() = {
    val z = x.map(_.toStrings)
    Try(z.reduce((l1,l2) => LemPos(l1.lemma + Settings.alternativePrint + l2.lemma, l1.pos +  Settings.alternativePrint + l2.pos)))
    match {
      case Success(x) => x
      case Failure(z) => LemPos("x", "y")
    }
  }
}

case class Clitic(x: Seq[Annotation]) extends Annotation
{
  def featureStructure(): NodeSeq =
  {
    x.flatMap(_.featureStructure()).map(f => f.asInstanceOf[Elem]).zipWithIndex.map(
      { case (e,n) =>
        e.copy(attributes=e.attributes.append(new UnprefixedAttribute("n", n.toString , Null)))
      }
    )
  }


  def toStrings() = {
    val z = x.map(_.toStrings)
    Try (z.reduce((l1,l2) => LemPos(l1.lemma + Settings.multivalSepPrint + l2.lemma, l1.pos +  Settings.multivalSepPrint + l2.pos)))
    match {
      case Success(x) => x
      case Failure(z) => LemPos("x", "y")
    }
  }
}

object TagStuff {
  val tagMapCGNStyle = Map(
    "NOU-C" -> "N(soort)",
    "NOU" -> "N(soort)",  "NOUEN" -> "N(soort)",
    "NEPER" -> "N(eigen,per)", "GEB.WENDEL" -> "N(eigen,per)",
    "PER" -> "N(eigen,per)",
    "NELOC" -> "N(eigen,loc)",
    "NEOTHER" -> "N(eigen,overig)",
    "NEORG" -> "N(eigen,org)",
    "CON" -> "VG",
    "VRB" -> "WW", "VRN" -> "WW",
    "ADP" -> "VZ",
    "ADJ" -> "ADJ",
    "ADV" -> "BW",
    "PRN" -> "VNW",
    "PR" -> "VNW",
    "ART" -> "LID", "RT" -> "LID",
    "NUM" -> "TW",
    "INT" -> "TSW",
    "RES" -> "SPEC",
    "FOREIGN" -> "SPEC(vreemd)",
    "UNRESOLVED" -> "SPEC(onverst)"
  )

  val tagMapCHNStyle = Map(
    "NOU-C" -> "NOU-C",
    "NOU" -> "NOU-C",  "NOUEN" -> "NOU-C",
    "NEPER" -> "NOU-P(type=per)", "GEB.WENDEL" -> "NOU-P(type=per)",
    "PER" -> "NOU-P(type=per)",
    "NELOC" -> "NOU-P(type=loc)",
    "NEOTHER" -> "NOU-P(type=other)",
    "NEORG" -> "NOU-P(type=per)",
    "CON" -> "CON",
    "VRB" -> "VRB", "VRN" -> "VRB",
    "ADP" -> "ADP",
    "ADJ" -> "ADJ", // Niet AA dus
    "ADV" -> "ADV",
    "PRN" -> "PD",
    "PR" -> "PD",
    "ART" -> "PD(type=art)", "RT" -> "PD(type=art)",
    "NUM" -> "NUM",
    "INT" -> "INT",
    "RES" -> "RES",
    "FOREIGN" -> "RES(type=foreign)",
    "UNRESOLVED" -> "RES(type=unknown)"
  )

  val tagMapTDNStyle = Map(
    "NOU-C" -> "NOU-C",
    "NOU" -> "NOU-C",  "NOUEN" -> "NOU-C",
    "NEPER" -> "NOU-P(type=per)", "GEB.WENDEL" -> "NOU-P(type=per)",
    "PER" -> "NOU-P(type=per)",
    "NELOC" -> "NOU-P(type=loc)",
    "NEOTHER" -> "NOU-P(type=oth)",
    "NEORG" -> "NOU-P(type=org)",
    "CON" -> "CONJ",
    "VRB" -> "VRB", "VRN" -> "VRB",
    "ADP" -> "ADP",
    "ADJ" -> "AA", // Hier wel AA dus
    "ADV" -> "ADV",
    "PRN" -> "PD(subtype=oth)",
    "PR" -> "PD(subtype=oth)",
    "ART" -> "PD(subtype=art)", "RT" -> "PD(subtype=art)",
    "NUM" -> "NUM",
    "INT" -> "INT",
    "RES" -> "RES(type=oth)",
    "FOREIGN" -> "RES(type=for)",
    "UNRESOLVED" -> "RES(type=uncl)"
  )

  val tagMapUDStyle = Map(
    "NOU-C" -> "NOUN",
    "NOU" -> "NOUN",
    "NOUEN" -> "NOUN",
    "NEPER" -> "PROPN",
    "GEB.WENDEL" -> "PROPN",
    "PER" -> "PROPN",
    "NELOC" -> "PROPN",
    "NEOTHER" -> "PROPN",
    "NEORG" -> "PROPN",
    "CON" -> "SCONJ|CCONJ",
    "VRB" -> "VERB|AUX", "VRN" -> "VERB",
    "ADP" -> "ADP",
    "ADJ" -> "ADJ", // Hier wel AA dus
    "ADV" -> "ADV",
    "PRN" -> "PRON",
    "PR" -> "PRON|DET",
    "ART" -> "DET", "RT" -> "DET",
    "NUM" -> "NUM",
    "INT" -> "INTJ",
    "RES" -> "X",
    "FOREIGN" -> "X",
    "UNRESOLVED" -> "X"
  )


  def parseLemPos(l: String, t: String): Annotation = {
    if (t.contains(Settings.alternativePrint)) {
      val ts = t.split(Settings.alternativeSep)
      val ls = l.split(Settings.alternativeSep)
      Alt(ls.zip(ts).map({case (l,t) => parseLemPos(l,t)}))
    }
    else if (t.matches(".*" + Settings.multivalSepSplit + ".*"))
    {
      val ts = t.split(Settings.multivalSepSplit)
      val ls = l.split(Settings.multivalSepSplit)
      Clitic(ls.zip(ts).map({case (l,t) => parseLemPos(l,t)}))
    } else LemPos(l,t)
  }

  def maakLemmaWeerGetal(w: String, l: String, possen: Seq[String]): String = // dit even uitschakelen, dat was voor Mike en Enrique...
  {
    val lemmata = l.split(multivalSepSplit)

    if (possen.count(_.contains("TW")) != 1)
      l
    else
    if (w.matches("[0-9]+")) // klopt niet!
    {
      val lNew = lemmata.zipWithIndex.map(
        { case (l, i)
        => if (possen(i).contains("TW") && !l.matches(".*(de|ste)$")) w else l }
      ).mkString(multivalSepSplit)
      //Console.err.println(s"Terug naar decimaal! $lNew voor ($w, $l, ${possen.mkString(multivalSepSplit)})")
      lNew
    }
    else
      l
  }

  def main(args: Array[String]): Unit = {
    val tags = List("NOU+PRN", "NOU|CONJ")
    val lemmata =  List("aap+noot", "aap|noot")
    lemmata.zip(tags).foreach( {case (l,t) =>
      val a = parseLemPos(l,t)
        println(s"[$l,$t] => $a ${a.toUD.toStrings}")
    })
  }
}
