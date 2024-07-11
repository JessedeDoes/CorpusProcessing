package corpusprocessing.papiaments.spelling

import scala.util.matching.Regex.Match

import java.text.Normalizer
import java.text.Normalizer.Form

object DiacriticRemover {
  def removeDiacritics(input: String): String = {
    val normalized = Normalizer.normalize(input, Form.NFD)
    normalized.replaceAll("\\p{InCombiningDiacriticalMarks}+", "")
  }
}

import DiacriticRemover._
object cw_aw {

  val baseDir = "/mnt/Projecten/Papiaments/Woordenboeken/ArubaLijst/"
  lazy val arubalijst = io.Source.fromFile(baseDir + "unieke_lijst.txt").getLines.map(x => x.replaceAll(" .*", "")).toSet
  lazy val alignments = io.Source.fromFile(baseDir + "../../Alignments/pairs.pruned.e2p.out").getLines
  val buki = baseDir + "../GoudenBoekje/buki_plus_retro.tab"
  lazy val bukiwords = io.Source.fromFile(buki).getLines.map(_.replaceAll("\t.*","")).filter(_.nonEmpty).toSet
  lazy val bukiwords_noacc = bukiwords.map(removeDiacritics)
  val replacements = List(
    "k" -> "c",
    "s" -> "c",
    "u" -> "o",
    "shon" -> "cion",
    "á" -> "a",
    "í" -> "i",
    "é" -> "e",
    "o" -> "ó",
    "t$" -> "d"
  )

  val andersom = List(
    "c"->"k",
    "c"->"s",
    "o" -> "u",
    "cio" -> "sho",
    "xio" -> "sho",
    "sci" -> "si",
    "xi" -> "si",
    "[pbk]s" -> "s",
    "ss" -> "s",
    "a" -> "á",
    "i" -> "í",
    "e" -> "é",
    "ó" -> "o",
    "ie" -> "e",
    "bs" -> "ps",
    "d$" -> "t",
    "cu" -> "kw",
    "b" -> "v"
  )

  def tryAll(source: String, pat: String, replacement: String) = {
    val r = new scala.util.matching.Regex(pat)
    val startingPoints = r.findAllMatchIn(source).map(_.start)
    val subsets = startingPoints.toSet.subsets()
    subsets.map(s => {
      val f: Match =>  String = m => if (s.contains(m.start)) replacement else m.matched
      r.replaceAllIn(source, f)
    }).toSet
  }

  def blaasop(V: Set[String], replacements: List[(String,String)] = replacements): Set[String] = {
    val stap = V.flatMap(w => replacements.flatMap({case (p,r) => tryAll(w,p,r) })).toSet // niet goed, moet alles wel of niet kunnen doen
    if ((stap diff V).nonEmpty) {
      blaasop(V ++ stap, replacements)
    } else V
  }

  def candidates(w: String): Set[String] = {
    if (arubalijst.contains(w)) Set() else blaasop(Set(w)).filter(x => arubalijst.contains(x))
  }


  def fromAruba() = {
    println(bukiwords_noacc.contains("farmaseutiko"))

    val matches = arubalijst.toList.sorted.map(w0 => {
      val w = removeDiacritics(w0)
      val cands = blaasop(Set(w), andersom) ++ Set(w)
      val found = (cands intersect bukiwords_noacc).headOption // .getOrElse("_")
      if (found.nonEmpty)
        println(w + " -> " + found.get)
      else {
        println(w + " -> " + "______") //   + cands)

      }
      w0 -> found
    })
  }

  def main(args: Array[String])  = fromAruba()

}

