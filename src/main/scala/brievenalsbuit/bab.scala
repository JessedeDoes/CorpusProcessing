package brievenalsbuit
import scala.xml._
import utils.PostProcessXML._
import java.io.File

import eindhoven.createWordids
import utils.ProcessFolder

import scala.io.Source._

object bab {
  val atHome = false
  val babDir = if (atHome) "/mnt/DiskStation/homes/jesse/work/BaB/" else
    "/mnt/Projecten/Taalbank/CL-SE-data/Corpora/LexiconToolCorpora/Zeebrieven/TKV/"

  val input = new File(babDir + "2.0")
  val output = new File(babDir + "2.2")
  val discardSubElemsOfWord = true
  val multivalSep = "⊕"


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

  val tagMap = Map(
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

  val overbodigeAttributen = Set("xtype", "time", "resp",  "subtype", "original", "type", "mform", "nform", "lemma") // misAlignent, changed


  def doW(w: Elem) =
  {
    val possen = (w \ "@type").text.split("_").toList
      .filter(_.matches(".*[A-Z].*"))
      .map(p => p.replaceAll("[?\\]]",""))

    val possen0 =   possen.map(p0 => p0.split("\\|").map(p => tagMap.getOrElse(p, s"POS_UNDEF:$p")).mkString("|"))

    val possen1 = if ((w \ "@n").nonEmpty) possen0.map(_.replaceAll("N.eigen", "SPEC(deeleigen")) else possen0

    if (possen1.exists(_.contains("UNDEF")))
      {
        Console.err.println(possen1)
        Console.err.println(w)
        System.exit(1)
      }



    val id = getId(w)

    val word = w.text

    val newLemma = (w \ "@lemma").text.replaceAll("_", multivalSep).toLowerCase()

    val lemmaNum = if (possen1.contains("TW") && (w \ "@n").isEmpty) maakLemmaWeerGetal(word, newLemma, possen1) else newLemma

    w.copy(child=if (discardSubElemsOfWord) Text(word) else w.child,
      attributes = w.attributes.filter(a => !overbodigeAttributen.contains(a.key))
        .append(new UnprefixedAttribute("pos", possen1.mkString(multivalSep), Null) )
        .append(new UnprefixedAttribute("type", possen.mkString(multivalSep), Null) )
        .append(new UnprefixedAttribute("lemma", lemmaNum, Null)))
  }

 //  <w n="0" pos="TW⊕TW(deel)" part="I" corresp="" lemma="drieëndertig⊕vierendertig " xml:id="w.71">3</w>
 // ook niet goed: Terug naar decimaal! 7 voor (7, zeveneneenhalf, TW) Dus altijd als deel dan blokkeren!

  def maakLemmaWeerGetal(w: String, l: String, possen: Seq[String]): String =
  {
    val lemmata = l.split(multivalSep)

    if (possen.count(_.contains("TW")) != 1)
      l
    else
    if (w.matches("[0-9]+")) // klopt niet!
    {

      val lNew = lemmata.zipWithIndex.map(
        { case (l, i)
           => if (possen(i).contains("TW") && !l.matches(".*(de|ste)$")) w else l }
      ).mkString(multivalSep)
      Console.err.println(s"Terug naar decimaal! $lNew voor ($w, $l, ${possen.mkString(multivalSep)})")
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
        val (part,partNumber,partAnalysis) = partAssignments(w)

        val word = w.text
        val partAttribute = new UnprefixedAttribute("part", part, Null)
        val nAttribute = new UnprefixedAttribute("n", partNumber.toString, Null)
        val oldPos = (w \ "@pos").text
        val newPos = if (partAnalysis.isEmpty || !oldPos.contains("WW")) if (oldPos.contains("deeleigen")) oldPos else addFeature(oldPos,"deel") else {
          val partDesc = if (word  == partAnalysis.get.verbalWordPart) "hoofddeel-ww" else "anderdeel-ww"
          addFeature(oldPos, partDesc)
        }
        val newPosAttribute = new UnprefixedAttribute("pos", newPos, Null)
        val newAtts = w.attributes.append( new UnprefixedAttribute("corresp", newCor, Null)).append(partAttribute)
        w.copy(attributes =  newAtts.filter(a => a.key != "n" && a.key != "pos").append(newPosAttribute).append(nAttribute))
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
