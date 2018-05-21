package eindhoven
import java.io.File
import java.text.Normalizer

import scala.xml._

case class Word(word: String, lemma: String, pos: String)
{
  def matches(mp: Int, sp: Int, ssp: Int):Boolean =
  {

    mp match {
      case 0 => if (List(0,2,8,9).contains(sp)) pos.startsWith("NOU-C") &&
        !(ssp == 0 && ! pos.contains("sg")) && !(ssp == 1 && ! pos.contains("pl"))
      else pos.startsWith("NOU-P")
      case 1 => pos.startsWith("AA")
      case 2 => pos.startsWith("VRB") && !((ssp == 5 || ssp == 6) && ! pos.contains("past")) &&
        !(List(1,2,3,4).contains(ssp) && ! pos.contains("pres")) &&
        !(List(0,1,2,3).contains(sp) && ! (pos.contains("=inf") || pos.contains("part")))
      case 3 => pos.startsWith("PD") &&
        !(List(2,3).contains(sp) && ! pos.contains("poss")) &&
        !(List(4,5).contains(sp) && ! pos.contains("ref")) &&
        !(List(0).contains(sp) && ! pos.contains("per"))
      case 4 => (pos.startsWith("PD") || pos.startsWith("NUM"))  && !(pos.contains("poss"))
      case 5 => pos.startsWith("ADV")
      case 6 => pos.startsWith("ADP")
      case 7 => pos.startsWith("CONJ")
      case 8 => pos.startsWith("INT")
      case _ => true
    }
  }
}

object Eindhoven {

  def noAccents(s: String):String = Normalizer.normalize(s, Normalizer.Form.NFD).replaceAll("\\p{M}", "").toLowerCase.trim
  val dir = "/home/jesse/data/Eindhoven"
  val files = new java.io.File(dir).listFiles.toStream.filter(f => f.getName().endsWith(("xml")))
  val goodies = scala.io.Source.fromFile(dir + "/" + "goede-woorden-uit-molex-postgres.csv").getLines.toList.map(l => l.split(";"))
    .map(r => r.map(_.trim.replaceAll(""""""", ""))).map(r => Word(r(1), r(0), r(2))
  )

  val goodMap:Map[String,List[Word]] = goodies.groupBy(w => noAccents(w.word))

  def updateElement(e: Elem, condition: Elem=>Boolean, f: Elem => Elem):Elem =
  {
    if (condition(e))
      f(e)
    else
      e.copy(child = e.child.map({
        {
          case e1: Elem => updateElement(e1,condition,f)
          case n:Node => n
        }
      }))
  }

  def doFile(f: File) =
  {
    val doc = XML.loadFile(f)
    (doc \\ "w").map(_.asInstanceOf[Elem])
    val d1 = updateElement(doc, _.label == "w", doWord)

    XML.save(f.getCanonicalPath + ".patch", d1, "UTF-8")
  }

  val m0 = <x y="1"/>.attributes.filter(p => p.key != "y")
  println(m0)

  def append(m: MetaData, l:List[UnprefixedAttribute]):MetaData =
  {
    val m1 = if (m==Null) m0 else m
    

    l.foldLeft(m1)( (m,u) => m.append(u))
  }

  import util.matching.Regex._

  def findVuPos(pos: String):(Int,Int,Int) =
  {
    val vuPos0:Int = "vu ([0-9]{3})".r.findFirstMatchIn(pos).map(m => m.group(1)).getOrElse("999").toInt

    val vuPos = vuPos0 % 1000
    val vuMainPos = (vuPos - vuPos % 100) / 100
    val vuSub1 = vuPos - 100 * vuMainPos
    val vuSubPos = (vuSub1 - vuSub1 % 10) / 10
    val vuSubSubPos = vuPos - 100 * vuMainPos - 10 * vuSubPos

    (vuPos, vuSubPos, vuSubSubPos)
  }

  def doWord(w: Elem):Elem = {
    val word = w.text

    val lemma0 = (w \\ "@lemma").text
    val lemma1 = if (lemma0 == "_") "" else lemma0

    val pos = (w \\ "@pos").text

    val (vuMainPos, vuSubPos, vuSubSubPos) = findVuPos(pos)

    val (lemma, supply):(String, Boolean) =  { if (lemma1 == "" && vuMainPos == 0 && vuSubSubPos == 0) (word,true); else (lemma1,false) }


    val lemmaIsWord = new UnprefixedAttribute("lemma", lemma, Null)

    //Console.err.println(s"$vuPos $vuMainPos:$vuSubPos:$vuSubSubPos")

    val cert: UnprefixedAttribute = new UnprefixedAttribute("maybenot", "true", Null)

    val candidates = goodMap.get(word.toLowerCase())

    val purgedWAttributes = w.attributes.filter(a => !(a.key=="lemma" && a.value.text=="_"))

    Console.err.println(purgedWAttributes)

    val extraAttributes: List[UnprefixedAttribute] =
      if (candidates.isDefined) {
        val c = candidates.get.filter(w => lemma.isEmpty || w.lemma == lemma)
        val lemmaCandidates = c.filter(w => lemma.isEmpty && w.matches(vuMainPos, vuSubPos, vuSubSubPos)).map(_.lemma).toSet

        val lemmaAttribute = new UnprefixedAttribute("lemma", lemmaCandidates.mkString("|"), Null)
        val lAdd = if (lemmaCandidates.isEmpty)
          if (supply) List(lemmaIsWord) else List()
        else List(lemmaAttribute)

        val withAccent = c.filter(w => noAccents(w.word) != w.word.toLowerCase())
        val withoutAccent = c.filter(w => noAccents(w.word) == w.word.toLowerCase())

        if (withAccent.nonEmpty) {
          val a0: UnprefixedAttribute = new UnprefixedAttribute("corr", withAccent.head.word, Null)

          Console.err.println(s"$word ($lemma) => $withAccent")
          if (withoutAccent.isEmpty) List(a0) ++ lAdd
          else {
            List(a0, cert) ++ lAdd
          }
        } else lAdd
      } else if (supply) List(lemmaIsWord) else List.empty[UnprefixedAttribute]
    w.copy(attributes = append(purgedWAttributes,extraAttributes))
  }

  def main(args: Array[String]) =
  {
    val d = new File(dir)
    d.listFiles.foreach(println)
    files.foreach(doFile)
  }
}

// leeuwenberg et al 2016 minimally supervised approach for  synonym extraction with word embeddings