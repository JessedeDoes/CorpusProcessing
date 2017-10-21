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

  def main(args: Array[String]):Unit =
  {
    val t = new SparqlToXquery(Mappings.testje)
    val q =
      s"""prefix : <http://example.org/>
        |prefix ud: <${Mappings.udPrefix}>
        |select ?s  ?o ?vl where
        |{
        | ?x :rel_su ?s .
        | ?s :lemma "mens" .
        | ?x :rel_obj1 ?o .
        | ?o :lemma ?w .
        | values ?w  {"zich"} .
        | ?x :child ?c .
        | ?c :pos "verb" .
        | ?c :lemma ?vl
        | }
      """.stripMargin

    val q1 =
      s"""prefix : <http://example.org/>
         |prefix ud: <${Mappings.udPrefix}>
         |select ?tonderwerp ?tgezegde ?tlv ?tmv where
         |{
         | ?gezegde ud:nsubj ?onderwerp .
         | ?gezegde ud:obj ?lv .
         | ?gezegde ud:iobj ?mv .
         |
         | ?gezegde :text ?tgezegde .
         | ?onderwerp :text ?tonderwerp .
         | ?lv :text ?tlv .
         | ?mv :text ?tmv
         | }
      """.stripMargin

    val q2 =  s"""prefix : <http://example.org/>
                 |prefix ud: <${Mappings.udPrefix}>
                 |select ?t1 ?t2 ?tcc where
                 |{
                 | ?c1 ud:conj ?c2 .
                 | ?c2 ud:cc ?cc .
                 | ?c1 :text ?t1 .
                 | ?c2 :text ?t2 .
                 | ?cc :text ?tcc
                 |}
      """.stripMargin

    val q3 = s"""prefix : <http://example.org/>
                |prefix ud: <${Mappings.udPrefix}>
                |select ?t1 ?t2 ?t3 where
                |{
                | ?c1 ud:xcomp ?c2 .
                | ?c1 :text ?t1 .
                | ?c2 :text ?t2 .
                | ?c2 :child ?ssub .
                | ?ssub ud:xcomp ?c3 .
                | ?c3 :text ?t3
                |}
      """.stripMargin

    val q4 = s"""prefix : <http://example.org/>
                |prefix ud: <${Mappings.udPrefix}>
                |select ?t1 ?t2  where
                |{
                | ?c1 ud:ccomp ?c2 .
                | ?c1 :text ?t1 .
                | ?c2 :text ?t2
                | }
      """.stripMargin


    val q5 =
      s"""prefix : <http://example.org/>
         |prefix ud: <${Mappings.udPrefix}>
         |select ?tonderwerp ?tgezegde ?tlv ?sent where
         |{
         | ?gezegde ud:nsubj ?onderwerp .
         | ?gezegde ud:obj ?lv .
         | ?lv :precedes ?onderwerp .
         | ?gezegde :text ?tgezegde .
         | ?onderwerp :text ?tonderwerp .
         | ?lv :text ?tlv .
         | ?gezegde :sentence ?sent
         | }
      """.stripMargin

    println(q5)

    val q6 =
      s"""
        |prefix ud: <${Mappings.udPrefix}>
        |SELECT *
        |WHERE {
        |
        |      ?pred ud:nsubj ?onderwerp .
        |
        |}
        |
      """.stripMargin
    val x:XQueryNode = t.translate(q6)
    println(x)
    println(x.toQuery())
    val bx = basex.BaseXConnection.default()
    val resultStream = bx.getAsScalaNodes(x.toQuery()).map(parseResult)
    println(QueryResults.response(resultStream.take(5)))
    //bx.ge.tAsScalaNodes(x.toQuery()).foreach(n => QueryResults.toJSON(parseResult(n)));
  }

  def maxResults = 10000

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