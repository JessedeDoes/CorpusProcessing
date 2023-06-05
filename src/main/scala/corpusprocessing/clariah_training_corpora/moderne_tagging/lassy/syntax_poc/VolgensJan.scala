package corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.syntax_poc
trait RelationOrToken {

}

trait Token extends RelationOrToken {}
trait Relation extends RelationOrToken  {

}

case class token(cql: String) extends Token {
  override def toString() = cql
}
case class rel(reltype: String = ".*", spanMode: String="'target'", direction:String="'both'") extends RelationOrToken
case class rspan(rel: RelationOrToken, spanMode: String) extends RelationOrToken
case class intersect(rels: Seq[RelationOrToken]) extends RelationOrToken {
  override def toString(): String = rels.map(_.toString).mkString(" & ")
}

object volgensJan {
  def setspan(r: RelationOrToken, spanMode: String): RelationOrToken = {
      r match {
        case sp: rspan if sp.spanMode == spanMode => sp
        case r: rel if r.spanMode == spanMode => r
        case _ => rspan(r, spanMode)
      }
  }

  def intersectIt(rels: Seq[RelationOrToken]): RelationOrToken = if (rels.size == 1) rels.head else intersect(rels)
}