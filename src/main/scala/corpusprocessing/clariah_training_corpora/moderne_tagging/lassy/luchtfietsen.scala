package corpusprocessing.clariah_training_corpora.moderne_tagging.lassy

import corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.luchtfietsen.subj_obj_iobj

import java.io.File
import scala.util.{Failure, Success, Try}

object Cartesian {
  def cross[S,T](X: Set[S], Y:Set[T]): Set[(S, T)] = X.flatMap(x => Y.map(y => x -> y))
}

import Cartesian._


trait ISpan {
  def sentence: UdSentence = ???
  def start: Int = ???
  def end: Int = ???
  def content = sentence.tokens.drop(start-1).take(end-start+1).map(_.FORM).mkString(" ")
  def showCaptures = captures.map(c => c._1 + "->"  + sentence.tokens(c._2-1).FORM).mkString(", ")
  def captures: Set[(String, Int, Int)] = Set()
  override def toString = s"$start--$end: [$content] {$showCaptures}"
}

trait IHeadedSpan extends ISpan {
  def head: Int = ???
  val headToken = sentence.tokens.filter(_.ID == head.toString).headOption
  // override def content = sentence.tokens.drop(start).take(end-start).map(t => if (t.ID == head.toString) s"<${t.FORM}:${t.DEPREL}>" else s"${t.FORM}:${t.DEPREL}").mkString(" ")
}

trait IHeadDepSpan extends IHeadedSpan {
  def dep: Int = ???
}

case class Span(override val sentence: UdSentence, override val start: Int, override val end: Int, override val captures: Set[(String, Int, Int)] = Set()) extends ISpan
case class HeadedSpan(override val sentence: UdSentence, override val start: Int, override val end: Int, override val head: Int, override val captures: Set[(String, Int, Int)] = Set()) extends IHeadedSpan {

}

case class HeadDepSpan(override val sentence: UdSentence, override val start: Int, override val end: Int, override val head: Int, override val dep: Int, override val captures: Set[(String, Int, Int)] = Set()) extends IHeadDepSpan {

  val depToken = sentence.tokens.filter(_.ID == dep.toString).head
  val rel = depToken.DEPREL

  override def toString = s"[${headToken.map(_.FORM).getOrElse("ROOT")} -> ${depToken.FORM}]; {$showCaptures}"
}


trait Query {
  // def matches(span: ISpan): Boolean
  def findMatches(s: Set[ISpan]) : Set[ISpan]
}


case class RelQuery(relName: String) extends Query {
  override def findMatches(s: Set[ISpan]): Set[ISpan] = luchtfietsen.rel(relName).findMatches(s)
}

case class BasicFilterQuery(filter: ISpan => Boolean) extends Query {
  //def matches(s : ISpan)  = filter(s)
  def findMatches(s: Set[ISpan]) = s.filter(filter).map({
    case hs: HeadedSpan => hs.copy(captures = Set(
        hs.headToken.map(x => ("headRel" + x.DEPREL, x.ID.toInt, x.ID.toInt))
    ).filter(_.nonEmpty).map(_.get))
    case ds: HeadDepSpan => ds.copy(captures = Set(
      ds.headToken.map(x => ("headRel:" + x.DEPREL, x.ID.toInt, x.ID.toInt)),
      Some(ds.depToken).map(x => (x.DEPREL, x.ID.toInt, x.ID.toInt))
    ).filter(_.nonEmpty).map(_.get))
  })
}

case class HeadIntersection(q1: Query, q2: Query) extends  Query {
   def findMatches(s: Set[ISpan])  = {
     val A = q1.findMatches(s).filter(_.isInstanceOf[IHeadedSpan]).map(_.asInstanceOf[IHeadedSpan])
     val B = q2.findMatches(s).filter(_.isInstanceOf[IHeadedSpan]).map(_.asInstanceOf[IHeadedSpan])

     cross[IHeadedSpan,IHeadedSpan](A,B).filter({case (a,b) => a.head == b.head}).map({case (a,b) =>
       // println(s"Join: $a with $b")
       val start = Math.min(a.start, b.start)
       val end = Math.max(a.end, b.end)
       HeadedSpan(a.sentence, start, end, a.head, captures = a.captures ++ b.captures)
     })
   }
}

case class HeadDepIntersection(q1: Query, q2: Query) extends  Query {
  def findMatches(s: Set[ISpan])  = {
    val A = q1.findMatches(s).filter(_.isInstanceOf[IHeadedSpan]).map(_.asInstanceOf[IHeadedSpan])
    val B = q2.findMatches(s).filter(_.isInstanceOf[IHeadDepSpan]).map(_.asInstanceOf[IHeadDepSpan])

    cross[IHeadedSpan,IHeadDepSpan](A,B).filter({case (a,b) => a.head == b.dep}).map({case (a,b) =>
      // println(s"Join: $a with $b")
      val start = Math.min(a.start, b.start)
      val end = Math.max(a.end, b.end)
      HeadedSpan(a.sentence, start, end, a.head, captures = a.captures ++ b.captures)
    })
  }
}

object luchtfietsen {

  implicit def relQuery(s: String)  = RelQuery(s)

  def createSpans(s: UdSentence) = {
    Try({
      s.tokens.map(t => {
        val pos = t.ID.toInt
        val headpos = t.HEAD.toInt
        val start = Math.min(pos, headpos)
        val end = Math.max(pos, headpos)
        HeadDepSpan(s, start, end, headpos, pos)
      }).toSet
    }) match {
      case Success(x) => Some(x)
      case _ => None
    }
  }

  lazy val alpino: Seq[Set[HeadDepSpan]] = alpino_to_huggingface.parseFile(new File("/home/jesse/workspace/UD_Dutch-Alpino/nl_alpino-ud-train.conllu")).map(createSpans).filter(_.nonEmpty).map(_.get)

  def find(q: Query) = alpino.map(s => {
    val matches = q.findMatches(s.map(_.asInstanceOf[ISpan]))
    matches
  }).filter(_.nonEmpty)

  def rel(relName: String) =  BasicFilterQuery(s => {
    s match {
      case x: HeadDepSpan => x.rel == relName
    }
  })

  val basic = RelQuery("amod")

  val subj_obj_iobj = HeadIntersection(HeadIntersection("nsubj", "obj"), "iobj")

  val subj_amod = HeadDepIntersection("amod", "nsubj")

  val testQueries = List(basic,subj_obj_iobj, subj_amod)

  def runQuery(q: Query,  max: Int=Integer.MAX_VALUE) = {
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
