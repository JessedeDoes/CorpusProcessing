package corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.syntax_poc

import corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.syntax_poc
import corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.syntax_poc.luchtfietsen.alpino

////////////////////////////////////////////////////////////////////////////////////////////////////////////

object Cartesian {
  def cross[S,T](X: Set[S], Y:Set[T]): Set[(S, T)] = X.flatMap(x => Y.map(y => x -> y))
}

import Cartesian._

object Queries {
  def find(q: Query, corpus: Seq[Set[ISpan]]) = corpus.map(s => {
    val matches = q.findMatches(s.map(_.asInstanceOf[ISpan]))
    matches
  }).filter(_.nonEmpty)

  def rel(relName: String) =  syntax_poc.BasicFilterQuery {
    case x: HeadDepSpan => x.rel == relName || relName == "*"
    case _ => false
  }

  def headIntersect(s: Seq[Query]): Query =  {
    if (s.size == 1) s.head else HeadIntersection(s.head, headIntersect(s.tail))
  }

  def headExtend(q: Query, relType: Option[String]  = None) = {
    val rq = RelQuery (relType.getOrElse ("*") )
    q match {
      case tq: TokenQuery => DepRestrict(rq, tq)
      case _ => HeadDepIntersection (q,  rq)
    }
  }

  // de head van q2 moet onder q1 hangen
  def headJoin(q1: Query, q2: Query, relType: Option[String]  = None) = {
    HeadIntersection(q1, headExtend(q2, relType))
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
      hs.headToken.map(x => ("headRel" + x.DEPREL, x.ID.toInt, x.ID.toInt))
    ).filter(_.nonEmpty).map(_.get))
    case ds: HeadDepSpan => ds.copy(captures = Set(
      ds.headToken.map(x => ("headRel:" + x.DEPREL, x.ID.toInt, x.ID.toInt)),
      Some(ds.depToken).map(x => (x.DEPREL, x.ID.toInt, x.ID.toInt))
    ).filter(_.nonEmpty).map(_.get))
    case ts: TokenSpan => ts
  })
}

trait TokenQuery extends Query {
  def &(q1: TokenQuery) = TokenAndQuery(this,q1)
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



case class HeadIntersection(q1: Query, q2: Query) extends  Query {
  def findMatches(s: Set[ISpan]): Set[ISpan] = {
    val A = q1.findMatches(s).filter(_.isInstanceOf[IHeadedSpan]).map(_.asInstanceOf[IHeadedSpan])
    val B = q2.findMatches(s).filter(_.isInstanceOf[IHeadedSpan]).map(_.asInstanceOf[IHeadedSpan])

    cross[IHeadedSpan,IHeadedSpan](A,B).filter({case (a,b) => a.head == b.head}).map({case (a,b) =>
      // println(s"Join: $a with $b")
      val start = Math.min(a.start, b.start)
      val end = Math.max(a.end, b.end)
      syntax_poc.HeadedSpan(a.sentence, start, end, a.head, captures = a.captures ++ b.captures)
    })
  }
}

case class HeadDepIntersection(q1: Query, q2: Query) extends  Query {
  // println(s"!!Head dep join of $q1 and $q2")
  def findMatches(s: Set[ISpan]): Set[ISpan] = {
    val A = q1.findMatches(s).filter(_.isInstanceOf[IHeadedSpan]).map(_.asInstanceOf[IHeadedSpan])
    val B = q2.findMatches(s).filter(_.isInstanceOf[IHeadDepSpan]).map(_.asInstanceOf[IHeadDepSpan])

    cross[IHeadedSpan,IHeadDepSpan](A,B).filter({case (a,b) => a.head == b.dep}).map({case (a,b) =>
      // println(s"Join: $a with $b")
      val start = Math.min(a.start, b.start)
      val end = Math.max(a.end, b.end)
      syntax_poc.HeadedSpan(a.sentence, start, end, b.head, captures = a.captures ++ b.captures)
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
        case hd: HeadDepSpan => syntax_poc.HeadDepSpan(a.sentence, start, end, a.head, hd.dep, captures = a.captures ++ b.captures)
        case _ => syntax_poc.HeadedSpan(a.sentence, start, end, a.head, captures = a.captures ++ b.captures)
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
      syntax_poc.HeadDepSpan(a.sentence, start, end, a.head, a.dep, captures = a.captures ++ b.captures)
    })
  }
}
