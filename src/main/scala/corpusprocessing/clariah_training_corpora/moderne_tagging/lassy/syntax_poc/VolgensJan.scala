trait RelationOrToken {

}

trait Token extends RelationOrToken {}
trait Relation extends RelationOrToken  {

}

case class token(pos: String) extends Token
case class rel(reltype: String = ".*", spanMode: String="target", direction:String="both") extends Relation
case class rspan(rel: Relation, spanMode: String) extends Relation
case class intersect(rels: List[RelationOrToken])