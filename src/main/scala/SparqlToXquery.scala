import org.openrdf.model.{Literal, URI, Value}
import org.openrdf.model.vocabulary.RDF
import org.openrdf.query.algebra._
import org.openrdf.query.parser.{ParsedQuery, QueryParserUtil}
import org.openrdf.query.{BindingSet, MalformedQueryException, QueryLanguage}
import org.openrdf.model.datatypes.XMLDatatypeUtil
import org.openrdf.query.algebra.{ValueExpr, Var}
import org.openrdf.query.algebra.{BindingSetAssignment, ExtensionElem, LeftJoin, Order, Projection, Reduced, SingletonSet, Slice, StatementPattern, TupleExpr}

//import it.unibz.inf.ontop.model.OBDAQueryModifiers.OrderCondition
import java.util.stream.{Collectors, StreamSupport}
import com.google.common.collect.{Iterables, Sets}
import com.google.common.collect.{ImmutableList, ImmutableSet}
import scala.collection.JavaConversions._


import Mappings._

case class TripleMapping(pm: PredicateMapping)
{
  def apply(sName: String, oName: String, predicate: String): SimpleSelect =
  {
    val base = pm(predicate)
    val m = Map("subject" -> sName, "object" -> oName)
    val mapped = base.renameVars(m)
    SimpleSelect(mapped.pathExpressions, mapped.variables, Set(sName, oName))
  }
}

object Mappings
{
  type PredicateMapping = Map[String, BasicPattern]
  val testMap:PredicateMapping = Map(
    "http://example.org/su" ->
      BasicPattern(
        Set("subject←//node[@cat='smain']", "object←$subject/node[@rel='su']")
      ),
    "http://example.org/ob" ->
      BasicPattern(
        Set("subject←//node[@cat='smain']", "object←$subject/node[@rel='obj1']")
      ),
    "http://example.org/word" ->
      BasicPattern(
        Set("object←$subject/@word")
      ),
    "http://example.org/lemma" ->
      BasicPattern(
        Set("object←$subject/@lemma")
      ),
    "http://example.org/pos" ->
      BasicPattern(
        Set("object←$subject/@pos")
      ),
    "http://example.org/rel" ->
      BasicPattern(
        Set("object←$subject/@rel")
      ),
    "http://example.org/child" ->
      BasicPattern(
        Set("object←$subject/*")
      ))
  val testje = TripleMapping(testMap)
}


class SparqlToXquery(basicMapping: TripleMapping) {

  var constNumber:Int=0

  def getNewConstantName():String = { constNumber += 1; "const" + constNumber}

  def getParsedQuery(sparql: String): ParsedQuery = {
    val parser = QueryParserUtil.createParser(QueryLanguage.SPARQL)
    val pq = parser.parseQuery(sparql, null)
    pq
  }

  def translate(q: String): XQueryNode =
  {
    val pq = getParsedQuery(q)
    translate(pq.getTupleExpr)
  }

  def translate(node: TupleExpr):XQueryNode =
  {
    System.err.println("translate:" + node)
    node match {
      case statement: StatementPattern => triplePattern(statement)
      case p: Projection => projection(p, translate(p.getArg))
      case join: Join =>
      {
        val l:XQueryNode = translate(join.getLeftArg)
        var r:XQueryNode = translate(join.getRightArg)
        (l,r) match {
          case (a:SimpleSelect, b:SimpleSelect) =>
            val newVars = a.variables ++ b.variables
            val newPaths = a.pathExpressions ++ b.pathExpressions
            val newSelectedVars = a.selected ++ b.selected
            SimpleSelect(newPaths, newVars, newSelectedVars,
              a.valueRestrictions ++ b.valueRestrictions)
          case (a:SimpleSelect, b:ValueRestrictionSet) =>
            a.copy(valueRestrictions =  a.valueRestrictions ++ b)
          case (b:ValueRestrictionSet, a:SimpleSelect) =>
            a.copy(valueRestrictions =  a.valueRestrictions ++ b)
        }
      }
      case ba: BindingSetAssignment =>
      {
         val x:Map[String,List[String]] = ba.getBindingSets.flatMap(
           bs => bs.getBindingNames.toList.map(n =>
             {
               val b = bs.getBinding(n)
               b.getName -> b.getValue
             })
         ).groupBy(_._1).mapValues(l => l.map(_._2.toString).toList)
        // println(x)
        new ValueRestrictionSet(x)
      }
      //case j: Join => join(j.getLeftArg, j.getRightArg)
      //case
    }
  }

  def projection(p: Projection, q: XQueryNode): XQueryNode =
  {
    val vars:Set[XQueryNode.Variable] = p.getProjectionElemList.getElements.map(e => e.getSourceName).toSet
    q match {
      case s: SimpleSelect => s.copy(selected=vars).asInstanceOf[XQueryNode]
      case b: BasicPattern => SimpleSelect(b.pathExpressions, b.variables, vars).asInstanceOf[XQueryNode]
    }
  }

  val dq = "\""

  def triplePattern(triple: StatementPattern):XQueryNode =
  {
    val sVar = triple.getSubjectVar
    val pVar = triple.getPredicateVar
    val oVar = triple.getObjectVar

    val (sVal,pVal,oVal) = (sVar.getValue,pVar.getValue,oVar.getValue)
    val (sName, pName, oName) = (
      if (sVal != null) getNewConstantName() else sVar.getName,
      pVar.getName,
      if (oVal != null) getNewConstantName() else oVar.getName)

    val vars = Set(sName, oName)

    if (pVal == null) // predicate variables not supported
      return null
    else {
      val x:SimpleSelect = basicMapping.apply(sName, oName, pVal.stringValue())
      val subjectRestriction = if (sVal != null)
        Map(sName -> List(dq + sVal.stringValue + dq))
      else Map.empty[String,List[String]]

      val objectRestriction = if (oVal != null)
        Map(oName -> List(dq + oVal.stringValue + dq))
      else Map.empty[String,List[String]]

      val r  = x.copy(valueRestrictions = ValueRestrictionSet(subjectRestriction ++ objectRestriction))
      println(r)
      return r
    }
    null
  }
}

object SparqlToXquery
{
  def main(args: Array[String]) =
  {
    val t = new SparqlToXquery(Mappings.testje)
    val q =
      """prefix : <http://example.org/>
        |select ?s  ?o ?vl where
        |{
        | ?x :su ?s .
        | ?s :lemma "mens" .
        | ?x :ob ?o .
        | ?o :lemma ?w .
        | values ?w  {"zich"} .
        | ?x :child ?c .
        | ?c :pos "verb" .
        | ?c :lemma ?vl
        | }
      """.stripMargin
    println(q)
    val x:XQueryNode = t.translate(q)
    println(x)
    println(x.toQuery())
  }
}

/**
  *
  *
Rels:
top
--
su
det
hd
vc
obj1
ld
mod
predc
mwp
cnj
crd
app
dp
cmp
body
pc
sat
nucl
svp
rhd
me
tag
obj2
whd
predm
dlink
se
sup
hdf
obcomp
pobj1

  Cats:

top
smain
np
ppart
pp
mwu
inf
conj
du
cp
sv1
ap
ssub
ti
rel
detp
oti
whq
whsub
advp
ppres
whrel
ahi
svan

Nuttige query:

  let $sv := for  $x in //node[@cat='smain'] ,
$o in $x/node[@rel='obj1'] ,
$s in $x/node[@rel='su']
return ($s/@lemma, $o/@lemma)
return distinct-values($sv)

  */
