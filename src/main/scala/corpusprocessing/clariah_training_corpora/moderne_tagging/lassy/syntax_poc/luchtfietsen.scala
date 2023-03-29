package corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.syntax_poc

import corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.{UdSentence, alpino_to_huggingface, syntax_poc}

import java.io.File
import scala.util.{Success, Try}


object luchtfietsen {

  implicit def relQuery(s: String)  = RelQuery(s)

  def createSpansForSentence(s: UdSentence): Option[Set[ISpan]] = {
    Try({
      s.tokens.map(t => {
        val pos = t.ID.toInt
        val headpos = t.HEAD.toInt
        val start = Math.min(pos, headpos)
        val end = Math.max(pos, headpos)
        HeadDepSpan(s, start, end, headpos, pos).asInstanceOf[ISpan]
      }).toSet ++ s.tokens.map(t => TokenSpan(s, t.ID.toInt))
    }) match {
      case Success(x) => Some(x)
      case _ => None
    }
  }

  lazy val alpino: Seq[Set[ISpan]] = alpino_to_huggingface.parseFile(new File("/home/jesse/workspace/UD_Dutch-Alpino/nl_alpino-ud-train.conllu")).map(createSpansForSentence).filter(_.nonEmpty).map(_.get)

  def find(q: Query) = alpino.map(s => {
    val matches = q.findMatches(s.map(_.asInstanceOf[ISpan]))
    matches
  }).filter(_.nonEmpty)

  def rel(relName: String) =  syntax_poc.BasicFilterQuery(s => {
    s match {
      case x: HeadDepSpan => x.rel == relName
      case _ => false
    }
  })

  def headIntersect(s: Seq[Query]): Query =  {
    if (s.size == 1) s.head else HeadIntersection(s.head, headIntersect(s.tail))
  }

  val regering_subject: DepRestrict = DepRestrict("nsubj", LemmaQuery("regering"))

  val besluiten_met_subject: HeadRestrict = HeadRestrict("nsubj", LemmaQuery("besluiten"))

  val minister_besluit: DepRestrict = DepRestrict(besluiten_met_subject,  LemmaQuery("minister"))

  val basic: RelQuery = RelQuery("amod")

  val subj_obj_iobj: Query = headIntersect(Seq("nsubj", "obj", "iobj", "compound:prt"))

  val subj_amod: HeadDepIntersection = HeadDepIntersection("amod", "nsubj")

  val testQueries: Seq[Query] = List(regering_subject, besluiten_met_subject, minister_besluit, basic,subj_obj_iobj, subj_amod)

  def runQuery(q: Query,  max: Int=Integer.MAX_VALUE): Unit = {
    println("\n\nQuery:" + q)
    find(q).take(max).foreach(s => {
      val sent = s.head.sentence.tokens.map(_.FORM).mkString(" ")
      println(s"\n###### $sent")
      s.foreach(x => println("\t" + x))
    })
  }

  def main(args: Array[String]) = {
    testQueries.foreach(q => runQuery(q,5))
  }
}
