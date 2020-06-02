package posmapping

import propositional.{And, Literal, Proposition}
import scala.util.matching.Regex
import scala.xml._

object UDTags {

   val udPoS = "data/UD/cpos.ud.txt"
   val udFeats = "data/UD/feat_val.ud.txt"

   val pos = io.Source.fromFile(udPoS).getLines.toList

   val partitions:Map[String,Set[String]] =
   {
     val a:Seq[(String,String)] = io.Source.fromFile(udFeats).getLines.toSeq.map(l => l.split("="))
       .map(l => l(0) -> l(1))
     a.groupBy(_._1).mapValues(l => l.map(_._2)).mapValues(_.toSet)
   }

  val pos2partitions:Map[String, List[String]]  = List( // this is language dependent
    "NOUN"    -> List("Abbr", "Animacy", "Case", "Gender", "Number", "X-Inflection"),
    "ADJ"  -> List("Abbr", "Case", "Gender", "Number", "Degree", "X-Inflection"),
    "VERB"   -> List("Abbr", "Mood", "Tense", "Number", "Person", "Polite", "VerbForm", "Voice", "Reflex",  "X-Inflection"),
    "NUM"  -> List("Abbr", "NumType", "Case", "Number", "Gender",  "X-Inflection"),
    "PRON"  -> List("Abbr", "PronType", "Case", "x-status", "Person",
      "Number", "Gender", "Degree",  "X-Inflection"),
    "DET"  -> List("Abbr", "PronType", "Case", "x-status", "Person",
      "Number", "Gender", "Degree", "X-Inflection"),
    "ADP"   -> List("Abbr"),
    "PART" -> List(),
    "AUX" ->  List("Abbr", "Mood", "Tense", "Number", "Person", "Polite", "VerbForm", "Voice", "Reflex",  "X-Inflection"),
    "SCONJ"   -> List("Abbr"),
    "CCONJ"   -> List("Abbr"),
    "ADV"   -> List("Abbr","Degree", "X-Inflection"),
    "INTJ"  -> List("Abbr"),
    "X" -> List("spectype"),
    "SYM" -> List()
  ).toMap
  //
  def main(args: Array[String]): Unit = {
    println(UDTagSet.toXML())
  }
}

import UDTagSet._

object UDTagSet extends TagSet("ud", UDTags.pos, UDTags.partitions, UDTags.pos2partitions)
{
  override def fromProposition(p:Proposition, posIsSpecial: Boolean = true):Tag =
  {
    val vars = p.varsIn
    val pos = vars.find(v => v.startsWith(s"${this.prefix}:pos=")).getOrElse("UNK").replaceAll(".*=", "")
    val features = vars.filter(f => !f.startsWith(s"${this.prefix}:pos=")).map(v => v.replaceAll(s"^${this.prefix}:", "")).mkString(",")
    new UDStyleTag(s"$pos($features)", this)
  }
  override def fromString(t: String): Tag = new UDStyleTag(t, this)
}


class UDStyleTag(tag: String, tagset: TagSet=defaultTagset) extends Tag(tag,tagset)
{
  //Console.err.println(s"Yes: $tag...")
  val Tag = new Regex("^([A-Z\\|-]+)\\((.*?)\\)")

  val Tag(pos,feats) = tag

  val featureValues:Array[String] = feats.split("\\s*,\\s*").filter(f => !(f.trim == ""))
  val features:List[Feature] = featureValues.map(parseFeature).toList ++ List(Feature("pos", pos))

  def parseFeature(f: String):Feature = {val c = f.split("\\s*=\\s*"); Feature(c(0), c(1))}

  def proposition:Proposition = And(features.map({case Feature(n,v) => Literal(s"${tagset.prefix}:$n=$v") } ) :_*)

  override def toString:String = tag
}