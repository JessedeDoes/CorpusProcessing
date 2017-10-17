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
  def apply(sName: String, oName: String, predicate: String): XQueryNode =
  {
    val base = pm(predicate)
    val m = Map("subject" -> sName, "object" -> oName)
    val mapped = base.renameVars(m)
    SimpleSelect(mapped.pathExpressions, mapped.variables, Set(sName, oName)).asInstanceOf[XQueryNode]
  }
}

object Mappings
{
  type PredicateMapping = Map[String, BasicPattern]
  val testMap:PredicateMapping = Map(
    "http://example.org/su" ->
      BasicPattern(
        Set("subject←//node[@cat='smain']", "object←$subject/node[@rel='su']"),
        Set("subject", "object"),
      ),
    "http://example.org/ob" ->
      BasicPattern(
        Set("subject←//node[@cat='smain']", "object←$subject/node[@rel='obj1']"),
        Set("subject", "object"),
      ))
  val testje = TripleMapping(testMap)
}


class SparqlToXquery(basicMapping: TripleMapping) {

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
            SimpleSelect(newPaths, newVars, newSelectedVars)
        }
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

  def triplePattern(triple: StatementPattern):XQueryNode =
  {
    val sVar = triple.getSubjectVar
    val pVar = triple.getPredicateVar
    val oVar = triple.getObjectVar

    val (sVal,pVal,oVal) = (sVar.getValue,pVar.getValue,oVar.getValue)
    val (sName, pName, oName) = (sVar.getName, pVar.getName, oVar.getName)
    val vars = Set(sName, oName)

    if (pVal == null) // predicate variables not supported
      return null
    else
      return basicMapping.apply(sName, oName, pVal.stringValue())
    null;
  }
}

object SparqlToXquery
{
  def main(args: Array[String]) =
  {
    val t = new SparqlToXquery(Mappings.testje)
    val q =
      """prefix : <http://example.org/>
        |select ?s  ?h where { ?x :su ?s . ?x :ob ?o . ?x :ob ?h }
      """.stripMargin
    println(q)
    val x:XQueryNode = t.translate(q)
    println(x)
    println(x.toQuery())
  }
}
