package corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.syntax_poc

import corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.syntax_poc
import corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.syntax_poc.luchtfietsen.alpino

import java.util

////////////////////////////////////////////////////////////////////////////////////////////////////////////

object Cartesian {
  def cross[S,T](X: Set[S], Y:Set[T]): Set[(S, T)] = X.flatMap(x => Y.map(y => x -> y))

  def crossList[S](x: Seq[Set[S]]):Set[Seq[S]] = {
    if (x.length == 1) {
      x.head.map(Seq(_))
    } else {
      val h = x.head
      val tailz = crossList(x.tail)
      h.flatMap(x => {
        tailz.map(t => x +: t)
      })
    }
  }

  def main(args: Array[String]) = {
    val example = Seq(Set(1,2), Set(2,3), Set(4,5))
    println(crossList[Int](example).toList.sortBy(_.toString()))
  }
}

import Cartesian._

import Condition._

object Queries {
  def find(q: Query, corpus: Seq[Set[ISpan]]) = corpus.map(s => {
    val matches = q.findMatches(s.map(_.asInstanceOf[ISpan]))
    matches
  }).filter(_.nonEmpty)

  def rel(relName: String) =  BasicFilterQuery {
    case x: HeadDepSpan => x.rel == relName || relName == "*"
    case _ => false
  }

  def headIntersect(s: Seq[Query], condition:ICondition = defaultCondition, postCondition:ICondition = trivial, label:String="unlabeled"): Query =  {
    SpanJoin(s,joinCondition=condition,postCondition = postCondition,label=label)
    // if (s.size == 1) s.head else HeadJoin(s.head, headIntersect(s.tail))
  }

  def headExtend(q: Query, relType: Option[String]  = None) = {
    val rq = RelQuery (relType.getOrElse ("*") )
    q match {
      case tq: TokenQuery => DepRestrict(rq, tq)
      case _ => HeadDepJoin (q,  rq)
    }
  }

  // de head van q2 moet onder q1 hangen
  def headJoin(q1: Query, q2: Query, relType: Option[String]  = None) = {
    HeadJoin(q1, headExtend(q2, relType))
  }



  /*
  head_extend(a: HeadedSpans, reltype: option[string]) -> HeadedSpans
Equivalent aan head_dep_intersect(a, <#reltype>)
Handig als opstapje voor de volgende
   */

}

import Queries._

// Pijlen omdraaien: a → b betekent hetzelfde als b ← a
trait Query {
  // def matches(span: ISpan): Boolean
  def findMatches(s: Set[ISpan]) : Set[ISpan]

  // Het grote raadsel: wat betekent het pijltje??

  def →(other: Query)  = {
    (this, other) match {
      case (t1: TokenQuery, t2: TokenQuery) => DepRestrict(HeadRestrict(RelQuery("*"), t1),t2).asInstanceOf[Query]
      case (t1: TokenQuery, t2: Query) => HeadRestrict(headExtend(t2), t1).asInstanceOf[Query]
      case (t1: Query, t2: Query) => headJoin(t1,t2).asInstanceOf[Query] // Hmm??????
    }
  }
}

case class BasicFilterQuery(filter: ISpan => Boolean) extends Query {
  def findMatches(s: Set[ISpan]): Set[ISpan] = s.filter(filter).map({
    case hs: HeadedSpan => hs.copy(captures = Set(
      hs.headToken.map(x => (x.DEPREL, x.ID.toInt, x.ID.toInt))
    ).filter(_.nonEmpty).map(_.get))
    case ds: HeadDepSpan => ds.copy(captures = Set(
      ds.headToken.map(x => (x.DEPREL, x.ID.toInt, x.ID.toInt)),
      Some(ds.depToken).map(x => (x.DEPREL, x.ID.toInt, x.ID.toInt))
    ).filter(_.nonEmpty).map(_.get))
    case ts: TokenSpan => ts
  })
}

trait TokenQuery extends Query {
  def &(q1: TokenQuery) = TokenAndQuery(this,q1)


  def toCQL(depth:Int = 0): String =  {
    this match {
      case LemmaQuery(l) => s"lemma='$l'"
      case PoSQuery(p) => s"pos='$p'"
      case TokenRelQuery(r) => s"rel='$r'"
      case TokenAndQuery(q1, q2) => s"${q1.toCQL()} & ${q2.toCQL()}"
      case EmptyTokenQuery() => ""
    }
  }

  def relPart: Option[TokenRelQuery] = {
    this match {
      case LemmaQuery(l) => None
      case PoSQuery(p) => None
      case TokenRelQuery(r) => Some(this.asInstanceOf[TokenRelQuery])
      case TokenAndQuery(q1, q2) => q1.relPart.toList.union(q2.relPart.toList).headOption
      case EmptyTokenQuery() => None
    }
  }

  def withoutRelPart : TokenQuery = {
    this match {
      case LemmaQuery(l) => this
      case PoSQuery(p) => this
      case TokenRelQuery(r) => EmptyTokenQuery()
      case TokenAndQuery(q1, q2) => (q1.withoutRelPart, q2.withoutRelPart) match {
        case (EmptyTokenQuery(), EmptyTokenQuery()) => EmptyTokenQuery()
        case (EmptyTokenQuery(), x) => x
        case (x, EmptyTokenQuery()) => x
        case (x,y) => TokenAndQuery(x,y)
      }
      case EmptyTokenQuery() => this
    }
  }
}

case class LemmaQuery(lemma: String) extends TokenQuery {
  def findMatches(s: Set[ISpan]): Set[ISpan] = BasicFilterQuery(s => s.isInstanceOf[TokenSpan] && s.firstToken.LEMMA == lemma).findMatches(s)
}

case class PoSQuery(lemma: String) extends TokenQuery {
  def findMatches(s: Set[ISpan]): Set[ISpan] = BasicFilterQuery(s => s.isInstanceOf[TokenSpan] && s.firstToken.UPOS == lemma).findMatches(s)
}

case class TokenRelQuery(rel: String) extends TokenQuery {
  def findMatches(s: Set[ISpan]): Set[ISpan] = BasicFilterQuery(s => s.isInstanceOf[TokenSpan] && s.firstToken.DEPREL == rel).findMatches(s)
}

case class EmptyTokenQuery() extends TokenQuery {
  def findMatches(s: Set[ISpan]): Set[ISpan] = s
}
case class TokenAndQuery(q1: TokenQuery, q2: TokenQuery)  extends TokenQuery  {
  def findMatches(s: Set[ISpan]): Set[ISpan] = q1.findMatches(s).intersect(q2.findMatches(s))
}

case class RelQuery(relName: String) extends Query {
  override def findMatches(s: Set[ISpan]): Set[ISpan] = rel(relName).findMatches(s)
}


// incorporate Optional parts....

case class SpanJoin(q: Seq[Query], joinCondition : ICondition = defaultCondition, postCondition: ICondition=Condition.trivial, label:String="unlabeled") extends Query {

  def findMatches(s: Set[ISpan]): Set[ISpan] =  {
    val A = q.map(_.findMatches(s).filter(_.isInstanceOf[IHeadedSpan]).map(_.asInstanceOf[IHeadedSpan]))
    val sequences: Set[Seq[IHeadedSpan]] = crossList[IHeadedSpan](A)

    sequences.filter(joinCondition.filter).flatMap(a => {
      val start = a.map(_.start).min
      val end =  a.map(_.end).max
      val captures = a.flatMap(x => x.captures).toSet
      val capturesx = a.zipWithIndex.flatMap({case (x,i) => x.captures.map({case (n,s,e) => (s"$label.$n",s,e)})}).toSet
      val unorderedResult = HeadedSpan(a.head.sentence, start, end, a.head.head, captures = captures)

      if (postCondition.filter(Seq(unorderedResult))) Set[HeadedSpan](unorderedResult) else Set[HeadedSpan]()

     })
    }
}

/*
Dit is niet helemaal wat je wil....
Nodig: diff op headniveau???
 */

case class QueryAndNot[T1 <: IHeadedSpan, T2 <: IHeadedSpan](q1: Query, q2: Query, projection: ISpan => Any = x => x.asInstanceOf[IHeadedSpan].head) extends Query {
  def findMatches(s: Set[ISpan]): Set[ISpan] = {
    val A = q1.findMatches(s)
    val B = q2.findMatches(s).map(_.asInstanceOf[IHeadedSpan]).map(projection)
    A.filter(x => !B.contains(projection(x)))
  }
}

case class HeadJoin(q1: Query, q2: Query) extends  Query {

  def findMatches(s: Set[ISpan]): Set[ISpan] = {
    SpanJoin(Seq(q1,q2)).findMatches(s)
  }

  def findMatches1(s: Set[ISpan]): Set[ISpan] = {
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

case class HeadDepJoin(q1: Query, q2: Query) extends  Query {
  // println(s"!!Head dep join of $q1 and $q2")
  def findMatches(s: Set[ISpan]): Set[ISpan] = {
    val A = q1.findMatches(s).filter(_.isInstanceOf[IHeadedSpan]).map(_.asInstanceOf[IHeadedSpan])
    val B = q2.findMatches(s).filter(_.isInstanceOf[IHeadDepSpan]).map(_.asInstanceOf[IHeadDepSpan])

    cross[IHeadedSpan,IHeadDepSpan](A,B).filter({case (a,b) => a.head == b.dep}).map({case (a,b) =>
      // println(s"Join: $a with $b")
      val start = Math.min(a.start, b.start)
      val end = Math.max(a.end, b.end)
      HeadedSpan(a.sentence, start, end, b.head, captures = a.captures ++ b.captures)
    })
  }
}

/*
Legt de restricties van tokenquery q2 op aan de head van de resultaten van q1 (die er dan ook nog moet zijn natuurlijk)
 */
case class HeadRestrict(q1: Query, q2: TokenQuery) extends  Query {
  def findMatches(s: Set[ISpan]): Set[ISpan] = {
    val A = q1.findMatches(s).filter(_.isInstanceOf[IHeadedSpan]).map(_.asInstanceOf[IHeadedSpan])
    val B = q2.findMatches(s).filter(_.isInstanceOf[TokenSpan]).map(_.asInstanceOf[TokenSpan])

    val x: Set[ISpan] = cross[IHeadedSpan,TokenSpan](A,B).filter({case (a,b) => a.head == b.start}).map({case (a,b) =>
      // println(s"Join: $a with $b")
      val start = Math.min(a.start, b.start)
      val end = Math.max(a.end, b.end)
      a match {
        case hd: HeadDepSpan => HeadDepSpan(a.sentence, start, end, a.head, hd.dep, captures = a.captures ++ b.captures)
        case _ => HeadedSpan(a.sentence, start, end, a.head, captures = a.captures ++ b.captures)
      }})

    if (false && A.size * B.size > 0) {
      println(s"###\n$q1")
      println(s"$q2")
      println(s"A:${A.size} B:${B.size} A=$A B=$B JOIN=$x")
    }
    x
  }
}

/*
Legt de restricties van tokenquery q2 op aan de dep van de resultaten van q1 (die er dan ook nog moet zijn natuurlijk)
 */
case class DepRestrict(q1: Query, q2: TokenQuery) extends  Query {
  def findMatches(s: Set[ISpan]): Set[ISpan] = {
    val A = q1.findMatches(s).filter(_.isInstanceOf[IHeadDepSpan]).map(_.asInstanceOf[IHeadDepSpan])
    val B = q2.findMatches(s).filter(_.isInstanceOf[TokenSpan]).map(_.asInstanceOf[TokenSpan])

    // println(B)

    cross[IHeadDepSpan,TokenSpan](A,B).filter({case (a,b) => a.dep == b.start}).map({case (a,b) =>
      // println(s"Join: $a with $b")
      val start = Math.min(a.start, b.start)
      val end = Math.max(a.end, b.end)
      HeadDepSpan(a.sentence, start, end, a.head, a.dep, captures = a.captures ++ b.captures)
    })
  }
}
