package posmapping
import java.io.FileWriter

import propositional.{And, Literal, Proposition}

import scala.util.Success
import scala.util.matching._
import scala.xml._


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
    "getal-n"  -> Set("getal-n", "mv-n", "ev-n", "zonder-n"),
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
    "spectype" -> Set("deeleigen", "vreemd", "afk", "afgebr", "symb", "meta", "onverst"),
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
    "TW"  -> List("numtype", "graad", "positie"),
    "SPEC" -> List("spectype")
  ).toMap

  def posTags = pos2partitions.keySet.toList

  lazy val lightFeaturesSet = Set("pos", "positie", "conjtype", "wvorm", "vwtype", "prontype", "pdtype")

  lazy val allTags:List[CGNTag] = scala.io.Source.fromFile("data/cgn.tagset").getLines().toList.sorted.map(CGNTag(_))

  case class TagWithExample(id: String, tag: String, example: String)
  {
    lazy val t = CGNTag(tag)
    lazy val liteTag = toLite(t).toString
    lazy val udTag = toUD(t).toString
  }

  def simplePoS(pos: String):String = pos.toString.replaceAll(",","_")
    .replace("()","").replace("(","_").replace(")","").toUpperCase
    .replace("ADV-PRON","ADV").replace("N_SOORT","N")

  def printEenLeukOverzichtje =
  {
    val x = tagsWithExamples.groupBy(_.liteTag)
    Console.err.println(x.keySet.size)
    val html =
      <html>
        <head>
        <meta http-equiv="content-type" content="text/html; charset=utf-8"></meta>
        </head>
      <body>
        <table>
        {x.keySet.toList.sorted.map(t => { val l = x(t)
          <tr valign="top"><td colspan="3" style="margin-top:4em; font-size:18pt"><b>{simplePoS(t)}</b></td></tr> ++
          l.map(te => <tr valign="top">
            <td style="margin-left:2em; font-size:8pt">{te.id}</td>
            <td style="font-size:8pt; margin: 0pt">{te.tag}</td>
            <td style="font-size:8pt; margin: 0pt">{te.udTag}</td>
            <td style="font-size:8pt; margin: 0pt"><i>{te.example}</i></td></tr>)
      })}
        </table>
      </body>
    </html>
    val p = new FileWriter("data/overzicht.html")
    p.write(html.toString)
    p.close()
  }

  def compacterOverzichtje =
  {
    val x = tagsWithExamples.groupBy(_.liteTag)
    val html =
      <html>
        <head>
        <meta http-equiv="content-type" content="text/html; charset=utf-8"></meta>
        </head>
        <body>
            {x.keySet.toList.sorted.map(t => { val l = x(t)
             <div>
            <h3>{simplePoS(t)}</h3>
               <p style="font-style: italic">{l.map(te => te.example).mkString(", ")}</p>
             </div>
          })}
        </body>
      </html>
    val p = new FileWriter("data/compact_overzicht.html")
    p.write(html.toString)
    p.close()
  }

  lazy val tagsWithExamples = scala.io.Source.fromFile("data/cgn_vb_vaneynde.utf8.txt").getLines()
    .map(l => l.replaceAll("#.*",""))
    .filter(l => !(l.trim.isEmpty))
    .map(l => l.split("\\t").toList)
    .filter(_.size >=3)
    .map(l => TagWithExample(l(0), l(1), l(2))).toList

  lazy val allLightTags0:List[CGNTag] = allTags.map(_.lightTag).toSet.toList

  lazy val allLightTags = allLightTags0.sortBy(_.toString)

  val mappingToUD = PropositionalTagsetMapping("data/cgn2ud.prop", ()=>CGNTagset, ()=>UDTagSet)
  val mappingToLite = PropositionalTagsetMapping("data/cgn2lite.prop", ()=>CGNTagset, ()=>CGNLiteTagset)

  def toUD(t: Tag):Tag = mappingToUD.mapTag(t)
  def toLite(t: Tag):Tag = mappingToLite.mapTag(t)

  def checkTagsWithExamples = tagsWithExamples.foreach(
    {case TagWithExample(id, tag, example) =>
       val t = CGNTag(tag)
        val tLight = toLite(t)
        val tUd = toUD(t)
        println(s"$id\t$t\t$tLight\t$tUd\t$example")
    }
  )

  def checkAllTags:Unit = allTags.foreach(t => {
    val p = t.proposition
    val tLight = toLite(t)
    val tUd = toUD(t)
    // val pLight = tLight.proposition

    val p1 = scala.util.Try(mappingToUD.mapToTagset(p)).get
    println(s"$t\t$tLight\t$tUd")
  })

  def checkLightTags:Unit = allLightTags.foreach(t => {
    val p = t.proposition
    val p1 = scala.util.Try(mappingToUD.mapToTagset(p)).get
    println(s"$t\t$p\t$p1")
  })

  def main(args: Array[String]):Unit = { printEenLeukOverzichtje; compacterOverzichtje}
  //Console.err.println("peek!!")
}

object CGNTagset extends TagSet("cgn", CGNPoSTagging.posTags, CGNPoSTagging.partitions, CGNPoSTagging.pos2partitions)
{
  //Console.err.println(s"blurk $posTags")

  override def fromProposition(p:Proposition, posIsSpecial: Boolean = false):Tag =
  {
    val vars = p.varsIn

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
    "VNW"  -> List("vwtype", "pdtype",  "positie", "graad"),
    "VZ"   -> List("vztype"),
    "VG"   -> List("conjtype"),
    "BW"   -> List(),
    "TSW"  -> List(),
    "TW"  -> List("numtype", "positie", "graad"),
    "LET"  -> List(),
    "SPEC" -> List("spectype")
  ).toMap

  def posTags = pos2partitions.keySet.toList
}

object CGNLiteTagset extends TagSet("cgnl", CGNLite.posTags, CGNLite.partitions, CGNLite.pos2partitions)
{
  override def fromProposition(p:Proposition, posIsSpecial: Boolean = false):Tag =
  {
    val vars = p.varsIn

    val pos = vars.find(v => v.startsWith(s"${this.prefix}:pos=")).getOrElse("UNK").replaceAll(".*=", "")
    val features = vars.filter(f => !f.startsWith(s"${this.prefix}:pos=")).toList
      .sortBy(f => {
        val n = f.replaceAll(s"^$prefix:feat.|=.*","")
        //Console.err.println(v)
        val l = pos2partitions(pos)
        val i = l.zipWithIndex.find({case (s,i) => s == n}).map({case (s,i) => i})
        i.getOrElse(0)
      })
      .map(v => v.replaceAll(s"^.*=", "")).mkString(",")

    var x = new CGNStyleTag(s"$pos($features)", this)
    //Console.err.println(s"Proposition to tag: $p->$x")
    x
  }
  override def fromString(t: String): Tag = CGNLiteTag(t)
}

object defaultTagset extends TagSet("unknown")
{
  override def fromProposition(p: Proposition, posIsSpecial: Boolean = true): Tag = ???

  override def fromString(t: String): Tag = ???
}



class CGNStyleTag(tag: String, tagset: TagSet) extends Tag(tag,tagset)
{
  val Tag = new Regex("^([A-Z]+)\\((.*?)\\)")

  if (Tag.findFirstIn(tag).isEmpty)
    {
      Console.err.println("PAS OP Mislukte tag: $tag")
    }

  val Tag(pos,feats) = if (Tag.findFirstIn(tag).isEmpty) "SPEC(overig)" else tag

  val featureValues:Array[String] = feats.split("\\s*,\\s*").filter(f => !(f.trim == ""))

  val features:List[Feature] = featureValues.map(parseFeature).toList ++ List(Feature("pos", pos))

  def parseFeature(f: String) = Feature(getFeatureName(f),f)

  def getFeatureName(f: String):String =
  {
    val p: String = this.pos
    val V: List[String] = this.tagset.inSubsets(f)

    val fn = if (V.isEmpty) {
      Console.err.println(s"Fatal error: tag cannot be parsed from tagset: $tag")
      System.exit(1)
      "UNK"
    }
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

