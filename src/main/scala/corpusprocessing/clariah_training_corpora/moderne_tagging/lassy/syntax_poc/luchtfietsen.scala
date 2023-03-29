package corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.syntax_poc

import corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.{UdSentence, alpino_to_huggingface, syntax_poc}

import java.io.File
import scala.util.{Success, Try}


import Queries._
import Spans._

object luchtfietsen {

  implicit def relQuery(s: String)  = RelQuery(s)



  lazy val alpino: Seq[Set[ISpan]] = alpino_to_huggingface.parseFile(new File("/home/jesse/workspace/UD_Dutch-Alpino/nl_alpino-ud-train.conllu")).map(createSpansForSentence).filter(_.nonEmpty).map(_.get)

  val regering_subject: DepRestrict = DepRestrict("nsubj", LemmaQuery("regering"))

  val besluiten_met_subject: HeadRestrict = HeadRestrict("nsubj", LemmaQuery("besluiten"))

  val minister_besluit: DepRestrict = DepRestrict(besluiten_met_subject,  LemmaQuery("minister"))

  val basic: RelQuery = RelQuery("amod")

  val subj_obj_iobj: Query = headIntersect(Seq("nsubj", "obj", "iobj", "compound:prt"))

  val subj_amod: HeadDepIntersection = HeadDepIntersection("amod", "nsubj")

  val minister_besluit_2 = LemmaQuery("besluiten") → LemmaQuery("minister")

  val noun_adj = PoSQuery("NOUN") → PoSQuery("ADJ")
  val verb_adv = PoSQuery("VERB") → PoSQuery("ADV")
  val wat_voor_mensen = LemmaQuery("mens") → PoSQuery("ADJ")

  val testQueries: Seq[Query] = List(wat_voor_mensen, minister_besluit_2, regering_subject, besluiten_met_subject, minister_besluit, basic,subj_obj_iobj, subj_amod)

  def runQuery(q: Query,  max: Int=Integer.MAX_VALUE): Unit = {
    println("\n\nQuery:" + q)
    find(q, alpino).take(max).foreach(s => {
      val sent = s.head.sentence.tokens.map(_.FORM).mkString(" ")
      println(s"\n###### $sent")
      s.foreach(x => println("\t" + x))
    })
  }

  def main(args: Array[String]) = {
    testQueries.foreach(q => runQuery(q,20))
  }
}
