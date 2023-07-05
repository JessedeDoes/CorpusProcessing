package corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.syntax_poc

import corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.conll_u.{UdSentence, UdToken}

import scala.util.{Success, Try}

object Spans {
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
}
////
trait ISpan {
  def sentence: UdSentence
  def start: Int
  def end: Int
  def token(i: Int): UdToken = sentence.tokens(i)
  def firstToken: UdToken = token(start-1)
  def content: String = sentence.tokens.slice(start - 1, start - 1 + end - start + 1).map(_.FORM).mkString(" ")
  def showCaptures: String = captures.map(c => c._1 + "->"  + sentence.tokens(c._2-1).FORM).mkString(", ")
  def captures: Set[(String, Int, Int)] = Set()
  override def toString = s"$start--$end: [$content] {$showCaptures}"
}

trait IHeadedSpan extends ISpan {
  def head: Int
  val headToken: Option[UdToken] = sentence.tokens.find(_.ID == head.toString)
  override def toString = s"span=($start-$end): [$content],\thead=${token(head-1).ID}:${token(head-1).FORM},\tcaptures={$showCaptures}"
  // override def content = sentence.tokens.drop(start).take(end-start).map(t => if (t.ID == head.toString) s"<${t.FORM}:${t.DEPREL}>" else s"${t.FORM}:${t.DEPREL}").mkString(" ")
}

trait IHeadDepSpan extends IHeadedSpan {
  def dep: Int
  // override def toString = s"$start--$end: [$content] , head=${token(head-1).ID}:${token(head-1).FORM} {$showCaptures}"
}

case class TokenSpan(override val sentence: UdSentence, override val start: Int) extends ISpan {
  override def end = start
}

case class Span(override val sentence: UdSentence, override val start: Int, override val end: Int, override val captures: Set[(String, Int, Int)] = Set()) extends ISpan


case class HeadedSpan(override val sentence: UdSentence, override val start: Int, override val end: Int, override val head: Int, override val captures: Set[(String, Int, Int)] = Set()) extends IHeadedSpan {

}

case class HeadDepSpan(override val sentence: UdSentence, override val start: Int, override val end: Int,
                       override val head: Int, override val dep: Int, override val captures: Set[(String, Int, Int)] = Set()) extends IHeadDepSpan {

  val depToken: UdToken = sentence.tokens.filter(_.ID == dep.toString).head
  val rel: String = depToken.DEPREL

  // override def toString = s"[${headToken.map(_.FORM).getOrElse("ROOT")} -> ${depToken.FORM}]; {$showCaptures}"
}

