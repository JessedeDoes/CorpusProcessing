package brievenalsbuit
import scala.xml._
import utils.PostProcessXML._
import java.io.File

import eindhoven.createWordids
import posmapping.CHNStyleTags
import utils.ProcessFolder
import utils.Tokenizer
import scala.io.Source._


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


  val tagMap = if (Settings.useCGNStyle) tagMapCGNStyle  else tagMapCHNStyle
  val overbodigeAttributen = Set("xtype", "time", "resp",  "subtype", "original", "type", "mform", "nform", "lemma") // misAlignent, changed


  def doW(w: Elem) =
  {
    val annotation = TagStuff.parseLemPos((w \ "@lemma").text, (w \ "@type").text).toCHN.map(x => x match
      {
        case LemPos(l,t) if  ((w \ "@n").nonEmpty && t.contains("NOU-P")) => LemPos(l, t.replaceAll("\\)", ",wordpart=proper)"))
        case _ => x
      }
    )

    /*
    if (!annotation.isInstanceOf[LemPos]) println(annotation)

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




    val newLemma = (w \ "@lemma").text.replaceAll("_", multivalSepSplit).toLowerCase()

    val lemmaNum = if (cgnPossen2.contains("TW") && (w \ "@n").isEmpty) maakLemmaWeerGetal(word, newLemma, cgnPossen2) else newLemma
    */


    val id = getId(w)

    val word = w.text

    val LemPos(lemma, pos) = annotation.toStrings
    val fs = annotation.featureStructure

    w.copy(child=(if (discardSubElemsOfWord) <seg>{word}</seg> else <seg>{w.child}</seg>) ++ fs,
      attributes = w.attributes.filter(a => !overbodigeAttributen.contains(a.key))
        //.append(new UnprefixedAttribute("msd", cgnPossen2.mkString(multivalSepPrint), Null) )
        .append(new UnprefixedAttribute("pos", pos, Null) )
        //.append(new UnprefixedAttribute("type", possen.mkString(multivalSepPrint), Null) )
        .append(new UnprefixedAttribute("lemma", lemma, Null)))
  }

 //  <w n="0" pos="TW⊕TW(deel)" part="I" corresp="" lemma="drieëndertig⊕vierendertig " xml:id="w.71">3</w>
 // ook niet goed: Terug naar decimaal! 7 voor (7, zeveneneenhalf, TW) Dus altijd als deel dan blokkeren!



  def tokenize(w: Elem):Seq[Node] =
  {
    //System.err.println("tokenize: " + w)
    val t = Tokenizer.tokenizeOne((w \ "seg").text)
    val seg = (w \ "seg").head.asInstanceOf[Elem]

    val nonTriv = (t.token.nonEmpty  && (t.leading.nonEmpty || t.trailing.nonEmpty))

    val word = w.text
    val lemma = (w \ "@lemma").text

    val safe = (!t.trailing.startsWith(".") || veiligePunten.contains(word,lemma)) && (discardSubElemsOfWord || seg.child.filter(_.isInstanceOf[Elem]).isEmpty)
    if (!safe || !nonTriv)
      w
    else
      {
        val r = (if (t.leading.nonEmpty) Seq(<pc>{t.leading}</pc>) else Seq()) ++
          Seq(w.copy( child = <seg>{t.token}</seg> ++ w.child.filter(_.label != "seg")  )) ++
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

        val newCHNPoS = if (partAnalysis.isEmpty || !oldCHNPoS.contains("VRB")) if (oldCHNPoS.contains("wordpart")) oldCHNPoS
                else addFeature(oldCHNPoS,"wordpart=part") else {
          val partDesc = if (word  == partAnalysis.get.verbalWordPart) "wordpart=vrb" else "wordpart=adv"
          addFeature(oldCHNPoS, partDesc)
        }

        val newCGNPosAttribute = new UnprefixedAttribute("msd", newCGNPoS, Null)

        val newCHPosAttribute = new UnprefixedAttribute("pos", newCHNPoS, Null)

        val newAtts = w.attributes.append( new UnprefixedAttribute("corresp", newCor, Null)).append(partAttribute)

        val annotation = parseLemPos((w \ "@lemma").text, newCHNPoS)

        val fs = annotation.featureStructure()
        w.copy(
          child  = w.child.filter(_.label != "fs") ++ fs,
          attributes =  newAtts.filter(a => a.key != "n" && a.key != "msd" && a.key != "pos").append(newCHPosAttribute).append(nAttribute))
      } else w
    }
    updateElement(d, _.label=="w", newCorresp)
  }

  def fixWeirdLinebreaksIn(e: Elem) = {
    val childElements = e.child.zipWithIndex.filter(_._1.isInstanceOf[Elem]).toSeq
    val dinges = childElements.zipWithIndex
    val fixes: Set[Int] = dinges.filter({case ((n, i),j) =>
        n.label == "lb" && (!((n \ "@rend").text == "lineBreak")) && ((j > 0 && childElements(j-1)._1.label=="lb")||
        (j > dinges.size -1 && childElements(j+1)._1.label=="lb"))  }).map(_._1._2).toSet
    val newChildren = e.child.zipWithIndex.map({case (n,i) => if (fixes.contains(i)) n.asInstanceOf[Elem].copy(label="pb") else n})
    e.copy(child = newChildren)
  }

  def doFile(in:String, out:String) = {
    val d = XML.load(in)
    val d1 = createWordids.createWordIds(d,false)
    val d2 = updateElement(d1, _.label=="w", doW)
    val d2b = updateElement(d2, _.label=="pc", simplifyPC)
    val d3 = updateElement2(d2b, _.label=="c", x => Seq(Text(" "))).asInstanceOf[Elem]
    val d4 = updateElement2(d3, _.label=="w", tokenize).head.asInstanceOf[Elem]
    val d5 = markWordformGroups(d4)
    val d6 = updateElement3(d5, e => true, fixWeirdLinebreaksIn)
    XML.save(out,d6,"UTF-8")
  }

  def main(args: Array[String]): Unit = {
    ProcessFolder.processFolder(input, output, doFile)
  }
}


