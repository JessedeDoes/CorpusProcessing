package brievenalsbuit
import scala.xml._
import utils.PostProcessXML._
import java.io.File
import utils.ProcessFolder
import scala.io.Source._

object bab {
  val babDir = "/mnt/DiskStation/homes/jesse/work/BaB/"
  val input = new File(babDir + "2.0")
  val output = new File(babDir + "2.1")
  val discardSubElemsOfWord = true
  val multivalSep = "⊕"

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

  val overbodigeAttributen = Set("xtype", "time", "resp",  "subtype", "original", "mform", "type", "nform", "lemma") // misAlignent, changed


  def doW(w: Elem) =
  {
    val possen = (w \ "@type").text.split("_").toList.filter(_.matches(".*[A-Z].*")).map(p => p.replaceAll("[?\\]]",""))
      .map(p0 => p0.split("\\|").map(p => tagMap.getOrElse(p, s"POS_UNDEF:$p")).mkString("|"))

    val possen1 = if ((w \ "@n").nonEmpty) possen.map(_.replaceAll("N.eigen", "SPEC(deeleigen")) else possen

    if (possen1.exists(_.contains("UNDEF")))
      {
        Console.err.println(possen1)
        Console.err.println(w)
        System.exit(1)
      }



    val id = getId(w)

    val word = w.text
    val newLemma = (w \ "@lemma").text.replaceAll("_", multivalSep)

    w.copy(child=if (discardSubElemsOfWord) Text(word) else w.child,
      attributes = w.attributes.filter(a => !overbodigeAttributen.contains(a.key))
        .append(new UnprefixedAttribute("pos", possen1.mkString(multivalSep), Null) )
        .append(new UnprefixedAttribute("lemma", newLemma, Null)))
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

  def fixEm(d: Elem):Elem =
  {
    val wordOrder = (d \\ "w").zipWithIndex.map({case (w,i) => w -> i}).toMap

    val stermatten = (d \\ "w").filter(x => (x \ "@n").nonEmpty).groupBy(e =>  (e \ "@n").text)
    // stermatten.values.foreach(l => Console.err.println(l.sortBy(e => (e \ "@n").text.toInt).map(show(_))))

    val partAssignments = stermatten.values.flatMap(l =>
    {
      val sorted:Seq[Node] = l.sortBy(wordOrder)
      val pos = (sorted.head \ "@pos").text
      if (Set("WW", "BW").contains(pos))
         Console.err.println(sorted.map(_.text).mkString(" ") + " / " + (sorted.head \ "@lemma").text + " /  " + pos )
      sorted.zipWithIndex.map({case (w,i) =>
          {
            val part = if (i == 0) "I" else if (i==l.length-1) "F" else "M"
            w -> part
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
        val part = partAssignments(w)
        val partAttribute = new UnprefixedAttribute("part", part, Null)
        val newAtts = w.attributes.append( new UnprefixedAttribute("corresp", newCor, Null)).append(partAttribute)
        w.copy(attributes =  newAtts.filter(_.key != "n"))
      } else w
    }
    updateElement(d, _.label=="w", newCorresp)
  }

  def doFile(in:String, out:String) = {
    val d = XML.load(in)
    val d1 = posmapping.createWordids.createWordIds(d)
    val d2 = updateElement(d1, _.label=="w", doW)
    val d2b = updateElement(d2, _.label=="pc", simplifyPC)
    val d3 = updateElement2(d2b, _.label=="c", x => Seq(Text(" "))).asInstanceOf[Elem]
    val d4 = updateElement2(d3, _.label=="w", tokenize).head.asInstanceOf[Elem]
    val d5 = fixEm(d4)
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
