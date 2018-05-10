package eindhoven
import java.io.File
import java.text.Normalizer

import scala.xml._

case class Word(word: String, lemma: String, pos: String)
{
  def matches(mp: Int):Boolean =
  {
    Console.err.println(mp)
    mp match {
      case 0 => pos.startsWith("NOU-C")
      case 1 => pos.startsWith("AA")
      case 2 => pos.startsWith("VRB")
      case 3 => pos.startsWith("PD")
      case 4 => pos.startsWith("PD") || pos.startsWith("NUM")
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

  def doWord(w: Elem):Elem = {
    val word = w.text
    val lemma = (w \\ "@lemma").text
    val pos = (w \\ "@pos").text

    val vuPos:Int = "vu ([0-9]{3})".r.findFirstMatchIn(pos).map(m => m.group(1)).getOrElse("999").toInt
    val vuMainPos = (vuPos - vuPos % 100) / 100


    val cert: UnprefixedAttribute = new UnprefixedAttribute("maybenot", "true", Null)

    val candidates = goodMap.get(word.toLowerCase())
    val extraAttributes: List[UnprefixedAttribute] =
      if (candidates.isDefined) {
        val c = candidates.get.filter(w => lemma.isEmpty || w.lemma == lemma)
        val lemmaCandidates = c.filter(w => lemma.isEmpty && w.matches(vuMainPos)).map(_.lemma).toSet

        val lemmaAttribute = new UnprefixedAttribute("lemma", lemmaCandidates.mkString("|"), Null)
        val lAdd = if (lemmaCandidates.isEmpty) List() else List(lemmaAttribute)

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
      } else List.empty[UnprefixedAttribute]
    w.copy(attributes = append(w.attributes,extraAttributes))
  }

  def main(args: Array[String]) =
  {
    val d = new File(dir)
    d.listFiles.foreach(println)
    files.foreach(doFile)
  }
}

// leeuwenberg et al 2016 minimally supervised approach for  synonym extraction with word embeddings