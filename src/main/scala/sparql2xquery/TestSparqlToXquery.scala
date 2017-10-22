package sparql2xquery

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

// https://daverog.wordpress.com/2013/06/04/the-enduring-myth-of-the-sparql-endpoint/
// kijk ook nog even naar http://citeseerx.ist.psu.edu/viewdoc/download?rep=rep1&type=pdf&doi=10.1.1.214.3631

import Mappings._
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
      else Map.empty[String, List[String]]

      val objectRestriction = if (oVal != null)
        Map(oName -> List(dq + oVal.stringValue + dq))
      else Map.empty[String, List[String]]

      val r  = x.copy(valueRestrictions = ValueRestrictionSet(subjectRestriction ++ objectRestriction))
      Console.err.println("triple pattern translated to:" + r)
      return r
    }
    null
  }
}

object SparqlToXquery
{
  def parseResult(n: scala.xml.Node):Map[String, scala.xml.Node] =
    (n \\ "var").map(v =>
    {
      val name = (v \\ "name").text
      val value = (v \\ "value").head
      name -> value
    }).toMap



  def maxResults = 100000

  def translateAndRunQuery(q: String):QueryResults.Response =
  {
    val t = new SparqlToXquery(Mappings.testje)
    val xqn = t.translate(q)
    val xquery = xqn.toQuery
    val bx = basex.BaseXConnection.default()
    val result = bx.getAsScalaNodes(xquery).take(maxResults).map(parseResult)
    val json = QueryResults.response(result)
    json
  }
}
