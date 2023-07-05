package corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.syntax_poc

import corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.{alpino_to_huggingface, syntax_poc}

import java.io.File
import scala.util.{Success, Try}


import Queries._
import Spans._
import corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.conll_u.UdSentence

import sext._

object luchtfietsen {

  implicit def relQuery(s: String)  = RelQuery(s)


  val alpino_file = "/home/jesse/workspace/UD_Dutch-Alpino/nl_alpino-ud-train.conllu"
  val lassy_small_file = "/home/jesse/workspace/UD_Dutch-LassySmall/nl_lassysmall-ud-train.conllu"
  val french_gsd_file = "/home/jesse/workspace/UD_French-GSD/fr_gsd-ud-train.conllu"

  val japanese_gsd_file  = "/home/jesse/workspace/UD_Japanese-GSD/ja_gsd-ud-train.conllu"
  val japanese_modern_file = "/home/jesse/workspace/UD_Japanese-Modern/ja_modern-ud-test.conllu"
  val japanese_ktc_file = "/home/jesse/workspace/UD_Japanese-KTC/ja_ktc-ud-train.conllu" // useless
  val japanese_bccwj_file = "/home/jesse/workspace/UD_Japanese-BCCWJ/ja_bccwj-ud-train.conllu" // useless! words are missing

  def connl2spans(f: String): Seq[Set[ISpan]] = alpino_to_huggingface.parseFile(new File(f)).map(createSpansForSentence).filter(_.nonEmpty).map(_.get)
  def connl2spans(filenames: Seq[String]): Seq[Set[ISpan]] = {
    println(s"Start loading from $filenames")
    val r = filenames.flatMap(f =>  {
      Console.err.println(s"Start loading $f")
      val rr = connl2spans(f)
      Console.err.println(s"Finished loading $f")
      rr
    })
    Console.err.println(s"Finished loading $filenames")
    r
  }


  lazy val alpino: Seq[Set[ISpan]] = connl2spans(alpino_file)
  lazy val lassy_small : Seq[Set[ISpan]] = connl2spans(lassy_small_file)

  lazy val french_gsd = connl2spans(french_gsd_file)
  lazy val japanese_gsd =  connl2spans(japanese_gsd_file)
  lazy val japanese_bccwj =  connl2spans(japanese_bccwj_file)
  lazy val japanese_combi =  connl2spans(Seq(japanese_gsd_file, japanese_modern_file))


  lazy val regering_subject: DepRestrict = DepRestrict("nsubj", LemmaQuery("regering"))

  lazy val besluiten_met_subject: HeadRestrict = HeadRestrict("nsubj", LemmaQuery("besluiten"))

  lazy val minister_besluit: DepRestrict = DepRestrict(besluiten_met_subject,  LemmaQuery("minister"))

  lazy val basic: RelQuery = RelQuery("amod")

  lazy val subj_obj_iobj: Query = headIntersect(Seq("nsubj", "obj", "iobj", "compound:prt"))

  lazy val subj_amod: HeadDepJoin = HeadDepJoin("amod", "nsubj")

  lazy val minister_besluit_2 = LemmaQuery("besluiten") → LemmaQuery("minister")

  lazy val noun_adj = PoSQuery("NOUN") → PoSQuery("ADJ")
  lazy val verb_adv = PoSQuery("VERB") → PoSQuery("ADV")
  lazy val wat_voor_mensen = PoSQuery("VERB") → (LemmaQuery("mens") → PoSQuery("ADJ"))

  lazy val wat_voor_mensen_deel = HeadRestrict(
    headExtend(PoSQuery("NOUN") → PoSQuery("ADJ")),
    PoSQuery("VERB"))


  val testQueries: Seq[Query] = Stream(wat_voor_mensen_deel) // , minister_besluit_2, regering_subject, besluiten_met_subject, minister_besluit, basic,subj_obj_iobj, subj_amod)

  def runQuery(q: Query,  max: Int=Integer.MAX_VALUE, treebank: Seq[Set[ISpan]] = alpino, printQuery: Boolean=false ): Unit = {
    if (printQuery) println("\n\nQuery:\n" + q.treeString.replaceAll("\\|", "\t"))

    val results: Seq[Set[ISpan]] = find(q, treebank)

    println(s"//// Results with naive evaluation: ${results.size} //////")

    results.take(max).foreach(s => {
      val sent = s.head.sentence.tokens.map(_.FORM).mkString(" ")
      println(s"\n$sent")
      s.foreach(x => println("\t" + x.toString.replaceAll("\\t", "\n\t")))
    })
  }

  def main(args: Array[String]) = {
    testQueries.foreach(q => runQuery(q,20))
  }
}
