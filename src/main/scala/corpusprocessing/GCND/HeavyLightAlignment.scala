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

case class HeavyLightAlignment(e: ElanAnnotation) {

  type token = Tokenizer.Token
  val a = new AlignmentGeneric(comp)
  //Console.err.println(e)

  lazy val orgTokens: List[Tokenizer.Token] = e.tekst_lv.replaceAll("#"," 畳").split("\\s+").toList.flatMap(Tokenizer.tokenizeErVanaf)
  lazy val ndlTokens: List[Tokenizer.Token] = e.tekst_zv.split("\\s+").toList.flatMap(Tokenizer.tokenizeErVanaf)

  def z(x: List[Tokenizer.Token]) = x.zipWithIndex.map({ case (y, i) => (i.toString, y) }) // x.flatMap(_.split("nnnxnxnxnxnxnxn")).zipWithIndex.map({ case (y, i) => (i.toString, y) })

  def align(): List[(token,token)] =  {

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

      lr.foreach(c => {
        val l = c._2.map(_._2).mkString(" ")
        val r = c._3.map(_._2).mkString(" ")
        println(s"${c._1 || c._2.size == c._3.size} $l <-> $r")
      })
    }
    if (orgTokens.size == ndlTokens.size &&  (ndlTokens.size == e.allAlpinoTokens.size || e.allAlpinoTokens.size == 0)) {
      orgTokens.zip(ndlTokens)
    } else {
      println(
        s"""
           |##### LV ${orgTokens.size}, ZV ${ndlTokens.size},  Alpino ${e.allAlpinoTokens.size} ######
           |${e.tekst_lv}\n${e.tekst_zv}\n${orgTokens.map(_.token)}\n${ndlTokens.map(_.token)}
           |Alpino tokens: ${e.allAlpinoTokens.size}. ${e.allAlpinoTokens.map(_._1)}""".stripMargin)
      matchIt()
      List()
    }
  }

  lazy val aligned = align()
}
