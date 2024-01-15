package corpusprocessing.GCND
import utils.Tokenizer
import utils.alignment.{AlignmentGeneric, SimOrDiff}

import java.io.File
import java.util.Comparator
import scala.xml._


object comp extends Comparator[(String,Tokenizer.Token)]
{
  type token = Tokenizer.Token
  import java.text.Normalizer

  def flatten(string : String) = Normalizer.normalize(string, Normalizer.Form.NFD).replaceAll("\\p{M}", "").replaceAll("[|*\\]\\[]","");

  override def compare(t: (String,token), t1: (String,token)):Int =
  {
    flatten(t._2.token).compareToIgnoreCase(flatten(t1._2.token))
  }
}

trait Tokenization {
  val orgTokens: List[Tokenizer.Token]
  val ndlTokens: List[Tokenizer.Token]
}
case class TokenizingDetach(e: ElanAnnotation) extends Tokenization {
  def fixhellip(s: String) = s.replaceAll("\\s*\\.\\.\\.\\.*", " ... ").replaceAll("\\s+", " ") // of wil je juist verplicht attachen?

  lazy val orgTokens: List[Tokenizer.Token] =
    fixhellip(e.tekst_lv)
      .replaceAll("#", " 畳")
      .split("\\s+")
      .toList
      .flatMap(Tokenizer.tokenizeErVanaf)
      .map(t => Tokenizer.Token(t.leading.replaceAll("畳", "#"), t.token.replaceAll("畳", "#"), t.trailing.replaceAll("畳", "#")))

  lazy val ndlTokens: List[Tokenizer.Token] =
    fixhellip(e.tekst_zv)
      .split("\\s+")
      .toList.flatMap(Tokenizer.tokenizeErVanaf)
}

case class TokenizingAttach(e: ElanAnnotation) extends Tokenization {
  def fixhellip(s: String) = s.replaceAll("\\s*\\.\\.\\.\\.*", "... ").replaceAll("\\s+", " ") // of wil je juist verplicht attachen?

  lazy val orgTokens: List[Tokenizer.Token] =
    fixhellip(e.tekst_lv)
      .replaceAll("#", " 畳")
      .split("\\s+")
      .toList
      .flatMap(Tokenizer.tokenize)
      .map(t => Tokenizer.Token(t.leading.replaceAll("畳", "#"), t.token.replaceAll("畳", "#"), t.trailing.replaceAll("畳", "#")))

  lazy val ndlTokens: List[Tokenizer.Token] =
    fixhellip(e.tekst_zv)
      .split("\\s+")
      .toList.flatMap(Tokenizer.tokenize)
}

case class HeavyLightAlignment(e: ElanAnnotation) {

  type token = Tokenizer.Token
  val a = new AlignmentGeneric(comp)
  //Console.err.println(e)

  lazy val tokenization: Tokenization = TokenizingAttach(e) // Attach werkt beter omdat bijvoorbeeld ... soms ontbreekt in de zware vernederlandsing

  val (orgTokens, ndlTokens) = tokenization.orgTokens -> tokenization.ndlTokens
  def z(x: List[Tokenizer.Token]) = x.zipWithIndex.map({ case (y, i) => (i.toString, y) }) // x.flatMap(_.split("nnnxnxnxnxnxnxn")).zipWithIndex.map({ case (y, i) => (i.toString, y) })

  def align(): (Boolean, List[(token,token)], String) =  {

    val o = z(orgTokens)
    val n = z(ndlTokens)

    def matchIt() =  {
      val chunks: Seq[SimOrDiff[(String, token)]] = a.findChunks(o, n)

      val lr: Seq[(Boolean, Seq[(String, token)], Seq[(String, token)], Int)] = chunks.map(
        c => {
          // Console.err.println(c)
          val left: Seq[(String, Tokenizer.Token)] = o.slice(c.leftStart, c.leftEnd)
          val right = n.slice(c.rightStart, c.rightEnd)
          (c.isSimilarity, left, right, c.leftStart)
        })

       lr.filter(c => !(c._2.size == c._3.size)).map(c => {
         val l = c._2.map(_._2).mkString(" ")
         val r = c._3.map(_._2).mkString(" ")
         val message = s"[$l] <-> [$r]"
         println(s"!!  $message")
         message
      })
    }

    val useAlpino = ndlTokens.size <= e.allAlpinoTokens.size;
    if (useAlpino) {
      (true,List(), "")
    } else {
      if (orgTokens.size == ndlTokens.size) {
        (false, orgTokens.zip(ndlTokens), "")
      } else {
        ElanStats.nopes = ElanStats.nopes+1
        println(
          s"""
             |##### LV ${orgTokens.size}, ZV ${ndlTokens.size},  Alpino ${e.allAlpinoTokens.size} ######
             |${e.tekst_lv}\n${e.tekst_zv}\n${orgTokens.map(_.toString)}\n${ndlTokens.map(_.toString)}
             |Alpino tokens: ${e.allAlpinoTokens.size}. ${e.allAlpinoTokens.map(_._1.text_zv).mkString(" ")}""".stripMargin)
        val messages = matchIt()
        (false,List(), messages.mkString("\n").replaceAll("--", "__"))
      }
    }
  }

  lazy val aligned = align()
}
