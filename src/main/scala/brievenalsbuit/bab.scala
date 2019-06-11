package brievenalsbuit
import scala.xml._
import utils.PostProcessXML._
import java.io.File

import eindhoven.createWordids
import posmapping.CHNStyleTags
import utils.ProcessFolder

import scala.io.Source._

trait Annotation {
  def featureStructure() : NodeSeq
  def valid(): Boolean = true
}

case class LemPos(lemma: String, pos: String)  extends Annotation {
  def featureStructure(): Elem =
  {
    val tag = CHNStyleTags.parseTag(pos)
    val features = CHNStyleTags.gysTagset.asTEIFeatureStructure(tag)
    val lem = <f name="lemma">{lemma}</f>
    features.copy(child = features.child ++ lem)
  }

  def valid() = 
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
}

object TagStuff {
    def parseLemPos(t: String, l: String): Annotation = {
      if (t.contains(Settings.alternativePrint)) {
        val ts = t.split(Settings.alternativeSep)
        val ls = l.split(Settings.alternativeSep)
        Alt(ts.zip(ls).map({case (t,l) => parseLemPos(t,l)}))
      }
      else if (t.contains(Settings.multivalSepSplit))
        {
          val ts = t.split(Settings.alternativeSep)
          val ls = l.split(Settings.alternativeSep)
          Clitic(ts.zip(ls).map({case (t,l) => parseLemPos(t,l)}))
        } else LemPos(t,l)
    }
}

import TagStuff._

object bab {
  import Settings._


  case class Analysis(word: String, lemmaParts: String, verbalWordPart: String, verbalLemmaPart: String)
  {

  }

  val wwAnalyses:Map[String,Analysis] = fromFile("data/bab/ww.analyse.txt").getLines()
    .map(l => l.split("\\t"))
    .map(l => Analysis(l(0), l(1), l(2), l(3)))
    .toList.groupBy(_.word).mapValues(_.head)

  val veiligePunten:Set[(String,String)] = fromFile("data/bab_veilige_punten.txt").getLines.map(l => l.split("\\t")).map(l => l(0) -> l(1)).toSet

  def getId(n: Node):Option[String] = n.attributes.filter(a => a.prefixedKey.endsWith(":id") ||
    a.key.equals("id")).map(a => a.value.toString).headOption

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
    "ADJ" -> "AA",
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

  val tagMap = if (Settings.useCGNStyle) tagMapCGNStyle  else tagMapCHNStyle
  val overbodigeAttributen = Set("xtype", "time", "resp",  "subtype", "original", "type", "mform", "nform", "lemma") // misAlignent, changed


  def doW(w: Elem) =
  {
    val annotation = TagStuff.parseLemPos((w \ "@type").text, (w \ "@lemma").text)

    val possen = (w \ "@type").text.split("_").toList
      .filter(_.matches(".*[A-Z].*"))
      .map(p => p.replaceAll("[?\\]]",""))

    val cgnPossen1 =   possen.map(p0 => p0.split("\\|").map(p => tagMapCGNStyle.getOrElse(p, s"POS_UNDEF:$p")).mkString("|"))
    val chnPossen1 =   possen.map(p0 => p0.split("\\|").map(p => tagMapCHNStyle.getOrElse(p, s"POS_UNDEF:$p")).mkString("|"))

    val cgnPossen2 = if ((w \ "@n").nonEmpty) cgnPossen1.map(_.replaceAll("N.eigen", "SPEC(deeleigen")) else cgnPossen1

    if (cgnPossen2.exists(_.contains("UNDEF")))
      {
        Console.err.println(cgnPossen2)
        Console.err.println(w)
        System.exit(1)
      }


    val id = getId(w)

    val word = w.text

    val newLemma = (w \ "@lemma").text.replaceAll("_", multivalSepSplit).toLowerCase()

    val lemmaNum = if (cgnPossen2.contains("TW") && (w \ "@n").isEmpty) maakLemmaWeerGetal(word, newLemma, cgnPossen2) else newLemma


    w.copy(child=if (discardSubElemsOfWord) Text(word) else w.child,
      attributes = w.attributes.filter(a => !overbodigeAttributen.contains(a.key))
        .append(new UnprefixedAttribute("msd", cgnPossen2.mkString(multivalSepPrint), Null) )
        .append(new UnprefixedAttribute("pos", chnPossen1.mkString(multivalSepPrint), Null) )
        .append(new UnprefixedAttribute("type", possen.mkString(multivalSepPrint), Null) )
        .append(new UnprefixedAttribute("lemma", lemmaNum, Null)))
  }

 //  <w n="0" pos="TW⊕TW(deel)" part="I" corresp="" lemma="drieëndertig⊕vierendertig " xml:id="w.71">3</w>
 // ook niet goed: Terug naar decimaal! 7 voor (7, zeveneneenhalf, TW) Dus altijd als deel dan blokkeren!

  def maakLemmaWeerGetal(w: String, l: String, possen: Seq[String]): String =
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
      Console.err.println(s"Terug naar decimaal! $lNew voor ($w, $l, ${possen.mkString(multivalSepSplit)})")
      lNew
    }
      else
      l
  }

  def tokenize(w: Elem):Seq[Node] =
  {
    val t = Tokenizer.tokenizeOne(w.text)
    val nonTriv = (t.token.nonEmpty  && (t.leading.nonEmpty || t.trailing.nonEmpty))

    val word = w.text
    val lemma = (w \ "@lemma").text

    val safe = (!t.trailing.startsWith(".") || veiligePunten.contains(word,lemma)) && (discardSubElemsOfWord || w.child.filter(_.isInstanceOf[Elem]).isEmpty)
    if (!safe || !nonTriv)
      w
    else
      {
        val r = (if (t.leading.nonEmpty) Seq(<pc>{t.leading}</pc>) else Seq()) ++
          Seq(w.copy( child = Text(t.token))) ++
          (if (t.trailing.nonEmpty) Seq(<pc>{t.trailing}</pc>) else Seq())
        r
      }
  }

  def idUnsafe(n: Node) = getId(n).get

  def simplifyPC(pc: Elem):Elem = <pc>{pc.text}</pc>

  def addFeature(pos: String, f: String) = if (pos.contains(")")) pos.replaceAll("[)]", s",$f)") else s"$pos($f)"

  def markWordformGroups(d: Elem):Elem =
  {
    val wordOrder = (d \\ "w").zipWithIndex.map({case (w,i) => w -> i}).toMap

    val stermatten = (d \\ "w").filter(x => (x \ "@n").nonEmpty).groupBy(e =>  (e \ "@n").text)
    // stermatten.values.foreach(l => Console.err.println(l.sortBy(e => (e \ "@n").text.toInt).map(show(_))))

    val partAssignments = stermatten.values.flatMap(l =>
    {
      val sorted:Seq[Node] = l.sortBy(wordOrder)
      val pos = (sorted.head \ "@pos").text
      val word = sorted.map(_.text).mkString(" ")
      val analysis = wwAnalyses.get(word)

      // if (Set("WW", "BW").contains(pos)) Console.err.println(word + " / " + (sorted.head \ "@lemma").text + " /  " + pos )

      sorted.zipWithIndex.map({case (w,i) =>
          {
            val part = if (i == 0) "I" else if (i==l.length-1) "F" else "M"
            w -> (part,i,analysis)
          }
      })
    }).toMap

    val sterMatMap = stermatten.mapValues(l => l.map(idUnsafe))

    def newCorresp(w: Elem): Elem = {
      if ((w \ "@n").nonEmpty)
      {
        val cor = (w \ "@n").text
        val id = idUnsafe(w)
        val setje = sterMatMap(cor).toSet.diff(Set(id))
        val newCor = setje.map(x => s"#$x").mkString(" ")
        val (part, partNumber, partAnalysis) = partAssignments(w)

        val word = w.text
        val partAttribute = new UnprefixedAttribute("part", part, Null)
        val nAttribute = new UnprefixedAttribute("n", partNumber.toString, Null)

        val oldCGNPoS = (w \ "@msd").text

        val newCGNPoS = if (partAnalysis.isEmpty || !oldCGNPoS.contains("WW")) if (oldCGNPoS.contains("deeleigen")) oldCGNPoS else addFeature(oldCGNPoS,"deel") else {
          val partDesc = if (word  == partAnalysis.get.verbalWordPart) "hoofddeel-ww" else "anderdeel-ww"
          addFeature(oldCGNPoS, partDesc)
        }

        val oldCHNPoS = (w \ "@pos").text

        val newCHNPoS = if (partAnalysis.isEmpty || !oldCHNPoS.contains("VRB")) if (oldCGNPoS.contains("deeleigen")) addFeature(oldCHNPoS,"wordpart=proper") else addFeature(oldCHNPoS,"wordpart=part") else {
          val partDesc = if (word  == partAnalysis.get.verbalWordPart) "wordpart=vrb" else "wordpart=adv"
          addFeature(oldCHNPoS, partDesc)
        }

        val newCGNPosAttribute = new UnprefixedAttribute("msd", newCGNPoS, Null)
        val newCHPosAttribute = new UnprefixedAttribute("pos", newCHNPoS, Null)
        val newAtts = w.attributes.append( new UnprefixedAttribute("corresp", newCor, Null)).append(partAttribute)
        w.copy(attributes =  newAtts.filter(a => a.key != "n" && a.key != "msd" && a.key != "pos").append(newCGNPosAttribute).append(newCHPosAttribute).append(nAttribute))
      } else w
    }
    updateElement(d, _.label=="w", newCorresp)
  }

  def doFile(in:String, out:String) = {
    val d = XML.load(in)
    val d1 = createWordids.createWordIds(d)
    val d2 = updateElement(d1, _.label=="w", doW)
    val d2b = updateElement(d2, _.label=="pc", simplifyPC)
    val d3 = updateElement2(d2b, _.label=="c", x => Seq(Text(" "))).asInstanceOf[Elem]
    val d4 = updateElement2(d3, _.label=="w", tokenize).head.asInstanceOf[Elem]
    val d5 = markWordformGroups(d4)
    XML.save(out,d5,"UTF-8")
  }

  def main(args: Array[String]): Unit = {
    ProcessFolder.processFolder(input, output, doFile)
  }
}

trait Tokenizer
{
  case class Token(leading:String, token:String, trailing:String)

  def tokenize(s:String): Array[Token]
}

object Tokenizer extends Tokenizer
{
  import scala.util.matching._
  val Split = new Regex("^(\\p{P}*)(.*?)(\\p{P}*)$")

  def tokenizeOne(s:String): Token =
  {
    val Split(l,c,r) = s
    Token(l,c,r)
  }

  def doNotTokenize(s:String): Token = Token("",s,"")

  override def tokenize(s:String): Array[Token] =
    s.split("\\s+").map(tokenizeOne)

  def main(args:Array[String]):Unit =
  {
    println(tokenize("The dog, i think, is 'hardly-' interesting??!").toList);
  }
}
