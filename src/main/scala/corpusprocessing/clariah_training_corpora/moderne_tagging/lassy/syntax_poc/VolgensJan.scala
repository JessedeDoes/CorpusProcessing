package corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.syntax_poc
trait RelationOrToken {

}

trait Token extends RelationOrToken {}
trait Relation extends RelationOrToken  {

}

case class token(cql: String) extends Token {
  override def toString() = cql
}
case class rel(reltype: String = ".*", spanMode: String="'target'", direction:String="'both'") extends Relation
case class rspan(rel: RelationOrToken, spanMode: String) extends Relation
case class intersect(rels: Seq[RelationOrToken]) extends RelationOrToken {
  override def toString(): String = rels.map(_.toString).mkString(" & ")
}