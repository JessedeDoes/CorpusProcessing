package corpusprocessing.papiaments.spelling

import java.io.PrintWriter
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

  val gerundio_list = baseDir + "gerundio_cw.txt" // "copy (select distinct wordform from paradigm where part_of_speech ~ '=ger'  and lemma_id in (select lemma_id from lemmata where language_id='pap_cw')) to '/tmp/gerundio.txt';"
  lazy val arubalijst = io.Source.fromFile(baseDir + "unieke_lijst.txt").getLines.map(x => x.replaceAll(" .*", "")).toSet.filter(!_.contains("["))
  lazy val alignments = io.Source.fromFile(baseDir + "../../Alignments/pairs.pruned.e2p.out").getLines
  val buki = baseDir + "../GoudenBoekje/buki_plus_retro.tab"

  lazy val bukiwords = io.Source.fromFile(buki).getLines.map(_.replaceAll("\t.*","")).filter(_.nonEmpty).toSet ++ io.Source.fromFile(gerundio_list).getLines.toSet


  lazy val approvedBukiWords =  io.Source.fromFile(buki).getLines.filter(_.contains("buki")).map(_.replaceAll("\t.*","")).filter(_.nonEmpty).toSet

  lazy val bukiwords_noacc = bukiwords.map(removeDiacritics)

  lazy val bukiReaccent: Map[String, Set[String]] = bukiwords.map(x => removeDiacritics(x) -> x).groupBy(_._1).mapValues(_.map(_._2))

  def prefer(s: Set[String], f: String => Boolean): Set[String]  = if (s.exists(f)) s.filter(f) else s


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
    "sio" -> "sho",
    "sci" -> "si",
    "xi" -> "si",
    "x" -> "ks",
    "x" -> "s",
    "th" -> "t",
    "[pbk]s" -> "s",
    // assimilaties
    "dh" -> "th",
    "dv" -> "tv",
    "bv" -> "pv",
    "bs" -> "ps",
    "nm" -> "mm",
    "iyo" -> "ilio",
    "^ps" -> "s",
    "ss" -> "s",
    "ie" -> "e",
    "au" -> "ou",
    "d$" -> "t",
    "cu" -> "kw",
    "b" -> "v",
    "dor$" -> "do",
    "y" -> "i"
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

  val N = 1000 // Integer.MAX_VALUE
  def pick(s: Set[String], n:Int=N) = util.Random.shuffle(s.toList).take(n).toSet

  def reconstruct(x: String): Set[String] = prefer(bukiReaccent(x), y => approvedBukiWords.contains(y)  )
  def fromAruba() = {

    val tsvTje = new PrintWriter("/tmp/match.tsv")

    val matches = pick(arubalijst).toList.sorted.map(w0 => {
      val w = removeDiacritics(w0)
      val cands = blaasop(Set(w), andersom) ++ Set(w)
      val found = (cands intersect bukiwords_noacc) // .getOrElse("_")
      if (found.nonEmpty) {
        val r = prefer(found.flatMap(reconstruct),x => approvedBukiWords.contains(x))
        println(w + " -> " + r.mkString(", "))
        tsvTje.println(s"$w\t${r.mkString(", ")}")
      } else {
        println(w + " -> " + "______") //   + cands)
        tsvTje.println(s"$w\t")
      }
      w0 -> found
    })
    val nMatches = matches.filter(_._2.nonEmpty).size
    val trivial = matches.filter({case (p,v) => v.contains(p)}).size
    val percentage = Math.round(10000 * (nMatches / matches.size.toDouble)).toDouble / 100
    println(s"matched $nMatches of ${matches.size} ($percentage), $trivial identical matches")
    tsvTje.close()
  }

  def main(args: Array[String])  = fromAruba()

}

