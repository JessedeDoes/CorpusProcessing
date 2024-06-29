package posmapping

import propositional.{And, Literal, Proposition}

import java.io.PrintWriter
import scala.util.matching.Regex
import scala.xml._

object UDTags {

   val udPoS = "data/UD/cpos.ud.txt"
   val udFeats = "data/UD/feat_val.ud.txt"

   val pos = io.Source.fromFile(udPoS).getLines.toList

   val partitions:Map[String,List[String]] =
   {
     val a:Seq[(String,String)] = io.Source.fromFile(udFeats).getLines.toSeq.map(l => l.split("="))
       .map(l => l(0) -> l(1))
     a.groupBy(_._1).mapValues(l => l.map(_._2)).mapValues(_.toSet).mapValues(_.toSet.toList)
   }

  val pos2partitions:Map[String, List[String]]  = List( // this is language dependent // huh? Propn?
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
  val pos = tag.replaceAll("upostag=([A-Za-z]+).*", "$1")
  val rest = tag.replaceAll("^.*?\\|", "")


  val featureValues:Array[String] = rest.split("\\s*\\|\\s*").filter(f => !(f.trim == ""))
  val features:List[Feature] = featureValues.map(parseFeature).toList.filter(x => x.name != "upostag" && x.name.toLowerCase.matches("^[a-z]+$")) // ++ List(Feature("pos", pos))
  println("\n#### "  + tag)
  features.foreach(println)

  def parseFeature(f: String):Feature = {val c = f.split("\\s*=\\s*"); Feature(c(0), c(1))}

  def proposition:Proposition = And(features.map({case Feature(n,v) => Literal(s"${tagset.prefix}:$n=$v") } ) :_*)

  override def toString:String = tag
}

object getTagsFromBlacklab  {
  val corpus = "UD_TEI_SHORTSENTENCES"
  //val url = "http://svotmc10.ivdnt.loc:8080/blacklab-server/parlamint_test_config/hits?first=0&group=hit%3Axpos&number=100000&patt=%5B%5D&adjusthits=yes&interface=%7B%22form%22%3A%22explore%22%2C%22exploreMode%22%3A%22frequency%22%7D"
  val url=s"http://svotmc10.ivdnt.loc:8080/blacklab-server/$corpus/hits?first=0&group=context%3Axpos%3Ai%3AH&number=10000&patt=%5B%5D"
  def main(args: Array[String]) = {
    println(url)
    val d = XML.load(url)
    val tags = (d \\ "hitgroup").filter(h => (h \ "size").text.toInt > 2)

      .map(x => (x  \ "identityDisplay").text)
      .toSet.filter(x => x.nonEmpty && !x.contains("(")).map(UDTagSet.fromString)
    tags.foreach(println)
    val tagset = TagSet.tagsetFromSetOfTags("pos", tags)

    val p = new PrintWriter("/tmp/tagset.json")
    p.println(tagset.asJSON.replaceAll("pos_upostag","pos_head"))
    p.close()
  }
}