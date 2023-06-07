package corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.syntax_poc

/*
Problemen: [lemma='er' & deprel='advmod'] & rel()
17 hits, zouden er meer moeten zijn

http://svotmc10.ivdnt.loc:8080/corpus-frontend/lassy-small/search/hits?first=0&number=20&patt=%5Blemma%3D%27er%27+%26+deprel%3D%27advmod%27%5D+%26+rel%28%29&interface=%7B%22form%22%3A%22search%22%2C%22patternMode%22%3A%22expert%22%7D


 Zie: https://github.com/INL/BlackLab/blob/feature/relations/doc/technical/design/plan-relations.md
 */


import corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.syntax_poc.luchtfietsen.{alpino, french_gsd, japanese_bccwj, japanese_combi, japanese_gsd, lassy_small}
import sext._

import scala.xml._
import RelationalQuery._

case class QueryNode(headProperties : TokenQuery,
                     children: Seq[QueryNode] = Seq(),
                     and_not: Seq[QueryNode] = Seq(),
                     condition: ICondition = Condition.defaultCondition,
                     postCondition: ICondition = Condition.trivial,
                     optional: Boolean = false,
                     label:String="unlabeled")
{

  lazy val depRel = headProperties.relPart.map(_.rel).map(x => s"'dep::${x}'").getOrElse("_")
  lazy val depRelAsTokenProperty = headProperties.relPart.map(_.rel).map(x => s"[deprel='$x']").getOrElse("[]")
  lazy val otherTokenStuff = token(s"[${headProperties.withoutRelPart.toCQL()}]")
  lazy val tokenPartCQL: Seq[RelationOrToken] = if (otherTokenStuff.cql.isEmpty || otherTokenStuff.cql == "[]") Seq() else Seq(otherTokenStuff)
  def nodeQuery(): Query = if (children.isEmpty) headProperties else {
    val clauses = children.map(x => x.nodeQuery()).map(x => Queries.headExtend(x))
    val negative_clauses = and_not.map(x => x.nodeQuery()).map(x => Queries.headExtend(x))
    val positive: HeadRestrict = HeadRestrict(Queries.headIntersect(clauses.toSeq, condition=condition, postCondition=postCondition, label=label), headProperties)

    if (negative_clauses.nonEmpty) {
      val negative =  HeadRestrict(Queries.headIntersect(negative_clauses.toSeq), headProperties)
      QueryAndNot(positive, negative)
    } else
      positive
  }

  def toPseudoCQL(depth:Int=0) : String = {
    val indent = ("\t" * depth)
    val stukjes = children.map(x => x.toPseudoCQL(depth+1)).map(x =>  x).mkString("")
    val relPart = headProperties.relPart.map(_.rel).mkString
    val stukjes_niet = {
      val p = and_not.map(x => x.toPseudoCQL(depth+2)).map(x =>   x)mkString(" ")
      if (p.isEmpty) "" else s"\n$indent\t!($p\n$indent\t)"
    }
    val tpp = headProperties.withoutRelPart
    val tp = tpp.toCQL(depth+1)
    val conditionPart = if (condition == Condition.defaultCondition) "" else "\n" + indent + "::" + condition.toString
    val postConditionPart = if (postCondition == Condition.trivial) "" else "\n" + indent + "::" + postCondition.toString
    val parts = List(tp,stukjes,stukjes_niet,conditionPart,postConditionPart).filter(_.nonEmpty).mkString(" ")
    val below = if (parts.nonEmpty) s"[${parts}]" else ""
    "\n" + (indent) + s"↦$relPart$below"
  }

  def toRelQuery(isRoot: Boolean = true): RelationOrToken = {
     val tokenPart = headProperties.toCQL()
     val hieronder: Seq[RelationOrToken] = children.map(_.toRelQuery(false)) // dit is nog niet goed
     if (children.isEmpty) {
       rel(depRel) ∩ tokenPartCQL
       // intersectIt(tokenPartCQL ++   Seq(rel(depRel, spanMode="'target'").asInstanceOf[RelationOrToken]))
     } else {
       val hieronder_sourced: Seq[RelationOrToken] = hieronder.map(x =>  (setspan(x, "'source'")))
       rel(depRel)  ∩  (tokenPartCQL ++ hieronder_sourced) // er gebeurt niets met relpart...
     }
  }
}

// [geneste haakjes? [pos="VERB" & lemma="fietsen" [rel="obj"] [rel="nsubj"]]
// vergelijk https://ufal.mff.cuni.cz/pmltqdoc/doc/pmltq_tutorial_web_client.html

/*
Problemen:17      de      de      DET     LID|bep|stan|rest       Definite=Def    18      det     18:det  _
18      Nederlandse     Nederlands      ADJ     ADJ|prenom|basis|met-e|stan     Degree=Pos      15      nmod    15:nmod:van     _
19      Bank    bank    NOUN    N|soort|ev|basis|zijd|stan      Gender=Com|Number=Sing  18      fixed   18:fixed        _

- Volgorde van dependents, ten opzichte van elkaar en ten opzichte van de head
- Meerdere dependents met zelfde rol (bijvoorbeeld drie ADJ bij een znw)
- 'Diepere' relaties (bijvoorbeeld  a ->* b voor b hangt willekeurig diep onder a, kan je constituenten mee maken)
- In plaats van alleen maar captures iets boom-achtigs in het resultaat meegeven?

Vergelijk cypher, https://neo4j.com/developer/cypher/querying/
 */

