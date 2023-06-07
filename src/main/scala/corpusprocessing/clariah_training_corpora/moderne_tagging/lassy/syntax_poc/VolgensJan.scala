package corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.syntax_poc
trait RelationOrToken {
  def ∩(x: RelationOrToken): RelationOrToken = RelationalQuery.intersectIt(Seq(this, x))
  def ∩(x: Seq[RelationOrToken]): RelationOrToken = RelationalQuery.intersectIt(this +: x)
  def &(x: RelationOrToken): RelationOrToken = RelationalQuery.intersectIt(Seq(this, x))

  def AND(x: RelationOrToken): RelationOrToken = RelationalQuery.intersectIt(Seq(this, x))
}

trait Token extends RelationOrToken {}
trait Relation extends RelationOrToken  {

}

case class token(cql: String) extends Token {
  override def toString() = cql
}
case class rel(reltype: String = "_", spanMode: String="'target'", direction:String="'both'") extends RelationOrToken
case class rspan(rel: RelationOrToken, spanMode: String) extends RelationOrToken
case class intersect(rels: Seq[RelationOrToken]) extends RelationOrToken {
  override def toString(): String = rels.map(_.toString).mkString(" & ")
}

object RelationalQuery {
  def setspan(r: RelationOrToken, spanMode: String): RelationOrToken = {
      r match {
        case sp: rspan if sp.spanMode == spanMode => sp
        case r: rel if r.spanMode == spanMode => r
        case _ => rspan(r, spanMode)
      }
  }

  def intersectIt(rels: Seq[RelationOrToken]): RelationOrToken = if (rels.size == 1) rels.head else intersect(rels.toSet.toSeq)
}