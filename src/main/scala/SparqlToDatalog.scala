import org.openrdf.query.MalformedQueryException
import org.openrdf.query.QueryLanguage
import org.openrdf.query.parser.ParsedQuery
import org.openrdf.query.parser.QueryParser
import org.openrdf.query.parser.QueryParserUtil
import it.unibz.inf.ontop.model.CQIE
import it.unibz.inf.ontop.model.DatalogProgram
import it.unibz.inf.ontop.model.Function
import it.unibz.inf.ontop.model.Term
import it.unibz.inf.ontop.model.Variable
import it.unibz.inf.ontop.model.ValueConstant
import it.unibz.inf.ontop.model.Constant

import it.unibz.inf.ontop.owlrefplatform.core.abox.SemanticIndexURIMap
import it.unibz.inf.ontop.owlrefplatform.core.basicoperations.UriTemplateMatcher
import it.unibz.inf.ontop.owlrefplatform.core.basicoperations.VocabularyValidator
import it.unibz.inf.ontop.owlrefplatform.core.translator.DatalogToSparqlTranslator
import it.unibz.inf.ontop.owlrefplatform.core.translator.SPARQLQueryFlattener
import it.unibz.inf.ontop.owlrefplatform.core.translator.SparqlAlgebraToDatalogTranslator
import it.unibz.inf.ontop.model.OBDAQueryModifiers._
import scala.collection.JavaConversions._
import org.openrdf.query.algebra.TupleExpr
import org.openrdf.model.Literal
import org.openrdf.model.URI
import org.openrdf.model.Value
import org.openrdf.model.datatypes.XMLDatatypeUtil
import org.openrdf.model.vocabulary.RDF
import org.openrdf.query.algebra._
import org.openrdf.query.algebra.ValueConstant
import org.openrdf.query.parser.ParsedGraphQuery
import org.openrdf.query.parser.ParsedQuery
import org.openrdf.query.parser.ParsedTupleQuery


// kijk naar: ../ontop/reformulation-core/src/main/java/it/unibz/inf/ontop/owlrefplatform/core/translator/SparqlAlgebraToDatalogTranslator.java

class SparqlToDatalog
{
  import org.openrdf.query.parser.ParsedQuery
  val translator = new SparqlAlgebraToDatalogTranslator(new UriTemplateMatcher, new SemanticIndexURIMap)


  @throws[Exception]
  def s2d(sparql: String): DatalogProgram =
  {
    val pq = getParsedQuery(sparql)
    translator.translate(pq).getProgram // let op signature is ook interessant
  }

  import com.google.common.collect.ImmutableList
  import com.google.common.collect.ImmutableSet
  import com.google.common.collect.Sets
  import it.unibz.inf.ontop.model.OBDAQueryModifiers
  //import it.unibz.inf.ontop.model.OBDAQueryModifiers.OrderCondition
  import it.unibz.inf.ontop.model.Term
  import it.unibz.inf.ontop.utils.ImmutableCollectors
  import org.openrdf.query.algebra.BindingSetAssignment
  import org.openrdf.query.algebra.ExtensionElem
  import org.openrdf.query.algebra.LeftJoin
  import org.openrdf.query.algebra.Order
  import org.openrdf.query.algebra.Projection
  import org.openrdf.query.algebra.ProjectionElem
  import org.openrdf.query.algebra.Reduced
  import org.openrdf.query.algebra.SingletonSet
  import org.openrdf.query.algebra.Slice
  import org.openrdf.query.algebra.StatementPattern
  import org.openrdf.query.algebra.TupleExpr
  import org.openrdf.query.algebra.ValueExpr
  import org.openrdf.query.algebra.Var
  import java.util
  import java.util.stream.Collectors
  import java.util.stream.StreamSupport

  import com.google.common.collect.ImmutableList
  import com.google.common.collect.ImmutableSet
  import com.google.common.collect.Iterables
  import com.google.common.collect.Sets
  import it.unibz.inf.ontop.model.OBDADataFactory
  import it.unibz.inf.ontop.model.Term
  import it.unibz.inf.ontop.model.impl.OBDADataFactoryImpl
  import it.unibz.inf.ontop.model.impl.OBDAVocabulary
  import org.openrdf.query.algebra.BindingSetAssignment
  import java.util.stream.Collectors

  private class TranslationResult(val atoms: ImmutableList[Nothing], val variables: ImmutableSet[Nothing], val isBGP: Boolean) {
    final val ofac = OBDADataFactoryImpl.getInstance

    /**
      * Extends the current translation result with bindings coming from {@link Extension} (expr AS ?x) or {@link BindingSetAssignment} (VALUES in SPARQL)
      *
      * @param bindings  a stream of bindings. A binding is a pair of a variable, and a value/expression
      * @param varMapper a function from bindings to { @link Variable}s
      * @param exprMapper a function maps a pair of a binding and a set variables to a { @link Term}
      * @param < T>        A class for binding. E.g. { @link org.openrdf.query.Binding} or { @link org.openrdf.query.algebra.ExtensionElem}
      * @return extended translation result
      */
    def extendWithBindings[T](bindings: Nothing, varMapper: Function[_ >: T, Nothing], exprMapper: BiFunction[_ >: T, ImmutableSet[Nothing], Term]): TranslationResult = {
      val vars = new Nothing(variables)
      val eqAtoms = bindings.map((b) => {
        def foo(b) = {
          val expr = exprMapper.apply(b, ImmutableSet.copyOf(vars))
          val v = varMapper.apply(b)
          if (!vars.add(v)) throw new IllegalArgumentException("Duplicate binding for variable " + v)
          ofac.getFunctionEQ(v, expr)
        }

        foo(b)
      })
      new TranslationResult(getAtomsExtended(eqAtoms), ImmutableSet.copyOf(vars), false)
    }

    def getAtomsExtendedWithNulls(allVariables: ImmutableSet[Nothing]): ImmutableList[Nothing] = {
      val nullVariables = Sets.difference(allVariables, variables)
      if (nullVariables.isEmpty) return atoms
      getAtomsExtended(nullVariables.stream.map((v: Nothing) => ofac.getFunctionEQ(v, OBDAVocabulary.NULL)))
    }

    /**
      * Extends the atoms in current translation result with {@code extension}
      *
      * @param extension a stream of functions to be added
      * @return extended list of atoms
      */
    def getAtomsExtended(extension: Nothing): ImmutableList[Nothing] = {
      ImmutableList.copyOf(Iterables.concat(atoms, extension.collect(Collectors.toList)))
      //            ImmutableList.Builder builder = ImmutableList.<Function>builder().addAll(atoms)
      //                    .addAll(nullVariables.stream().map(v -> ofac.getFunctionEQ(v, OBDAVocabulary.NULL)).iterator());
      //            return builder.build();
    }
  }

  private def translate(node: TupleExpr): Nothing = { //System.out.println("node: \n" + node);
    if (node.isInstanceOf[Slice]) { // SLICE algebra operation
      val slice = node.asInstanceOf[Slice]
      //val modifiers = program.getQueryModifiers
      //modifiers.setOffset(slice.getOffset)
      //modifiers.setLimit(slice.getLimit)
      return translate(slice.getArg)
    }
    else if (node.isInstanceOf[Distinct]) { // DISTINCT algebra operation
      val distinct = node.asInstanceOf[Distinct]
      // program.getQueryModifiers.setDistinct
      return translate(distinct.getArg)
    }
    else if (node.isInstanceOf[Reduced]) { // REDUCED algebra operation
      val reduced = node.asInstanceOf[Reduced]
      return translate(reduced.getArg)
    }
    else if (node.isInstanceOf[Order]) { // ORDER algebra operation
      val order = node.asInstanceOf[Order]
      // val modifiers = program.getQueryModifiers
      import scala.collection.JavaConversions._
      for (c <- order.getElements) {
        val expression = c.getExpr
        if (!expression.isInstanceOf[Var]) throw new IllegalArgumentException("Error translating ORDER BY. " + "The current implementation can only sort by variables. " + "This query has a more complex expression '" + expression + "'")
        val v = expression.asInstanceOf[Var]
        //val `var` = ofac.getVariable(v.getName)
        //val direction = if (c.isAscending) OrderCondition.ORDER_ASCENDING
        //else OrderCondition.ORDER_DESCENDING
        //modifiers.addOrderCondition(`var`, direction)
      }
      return translate(order.getArg)
    }
    else if (node.isInstanceOf[StatementPattern]) { // triple pattern
      return translateTriplePattern(node.asInstanceOf[StatementPattern])
    }
    else if (node.isInstanceOf[SingletonSet]) { // the empty BGP has no variables and gives a single solution mapping on every non-empty graph
      return new Nothing(ImmutableList.of, ImmutableSet.of, true)
    }
    else if (node.isInstanceOf[Join]) { // JOIN algebra operation
      val join = node.asInstanceOf[Join]
      val a1 = translate(join.getLeftArg)
      val a2 = translate(join.getRightArg)
      val vars = Sets.union(a1.variables, a2.variables).immutableCopy
      if (a1.isBGP && a2.isBGP) { // collect triple patterns into BGPs
        val atoms = ImmutableList.builder[Nothing].addAll(a1.atoms).addAll(a2.atoms).build
        return new Nothing(atoms, vars, true)
      }
      else {
        val body = ofac.getSPARQLJoin(wrapNonTriplePattern(a1), wrapNonTriplePattern(a2))
        return new Nothing(ImmutableList.of(body), vars, false)
      }
    }
    else if (node.isInstanceOf[LeftJoin]) { // OPTIONAL algebra operation
      val lj = node.asInstanceOf[LeftJoin]
      val a1 = translate(lj.getLeftArg)
      val a2 = translate(lj.getRightArg)
      val vars = Sets.union(a1.variables, a2.variables).immutableCopy
      val body = ofac.getSPARQLLeftJoin(wrapNonTriplePattern(a1), wrapNonTriplePattern(a2))
      val expr = lj.getCondition
      if (expr != null) {
        val f = getFilterExpression(expr, vars)
        body.getTerms.add(f)
      }
      return new Nothing(ImmutableList.of(body), vars, false)
    }
    else if (node.isInstanceOf[Nothing]) { // UNION algebra operation
      val union = node.asInstanceOf[Nothing]
      val a1 = translate(union.getLeftArg)
      val a2 = translate(union.getRightArg)
      val vars = Sets.union(a1.variables, a2.variables).immutableCopy
      val res = createFreshNode(vars)
      appendRule(res.atoms.get(0), a1.getAtomsExtendedWithNulls(vars))
      appendRule(res.atoms.get(0), a2.getAtomsExtendedWithNulls(vars))
      return res
    }
    else if (node.isInstanceOf[Nothing]) { // FILTER algebra operation
      val filter = node.asInstanceOf[Nothing]
      val a = translate(filter.getArg)
      val f = getFilterExpression(filter.getCondition, a.variables)
      val atoms = ImmutableList.builder[Nothing].addAll(a.atoms).add(f).build
      // TODO: split ANDs in the FILTER?
      return new Nothing(atoms, a.variables, false)
    }
    else if (node.isInstanceOf[Projection]) { // PROJECT algebra operation
      val projection = node.asInstanceOf[Projection]
      val sub = translate(projection.getArg)
      val pes = projection.getProjectionElemList.getElements
      // the two lists are required to synchronise the order of variables
      val sVars = new util.ArrayList[_](pes.size)
      val tVars = new util.ArrayList[_](pes.size)
      var noRenaming = true
      import scala.collection.JavaConversions._
      for (pe <- pes) {
        val sVar = ofac.getVariable(pe.getSourceName)
        if (!sub.variables.contains(sVar)) throw new IllegalArgumentException("Projection source of " + pe + " not found in " + projection.getArg)
        sVars.add(sVar)
        val tVar = ofac.getVariable(pe.getTargetName)
        tVars.add(tVar)
        if (!sVar.equals(tVar)) noRenaming = false
      }
      if (noRenaming && sVars.containsAll(sub.variables)) { // neither projection nor renaming
        return sub
      }
      val vars = ImmutableSet.copyOf(tVars.stream.map((t) => t.asInstanceOf[Nothing]).collect(Collectors.toSet))
      if (noRenaming) return new Nothing(sub.atoms, vars, false)
      val head = getFreshHead(sVars)
      appendRule(head, sub.atoms)
      val atom = ofac.getFunction(head.getFunctionSymbol, tVars)
      return new Nothing(ImmutableList.of(atom), vars, false)
    }
    else if (node.isInstanceOf[Extension]) { // EXTEND algebra operation
      val extension = node.asInstanceOf[Extension]
      val sub = translate(extension.getArg)
      val nontrivialBindings = extension.getElements.stream.filter // ignore EXTEND(P, v, v), which is sometimes introduced by Sesame SPARQL parser
      (ee) =>
        !(ee.getExpr.isInstanceOf[Var] && ee.getName.equals((ee.getExpr.asInstanceOf[Var]).getName))
        return sub.extendWithBindings(nontrivialBindings, (ee) => ofac.getVariable(ee.getName), (ee, vars) => getExpression(ee.getExpr, vars))
    }
    else if (node.isInstanceOf[BindingSetAssignment]) { // VALUES in SPARQL
      val values = node.asInstanceOf[BindingSetAssignment]
      val empty = new Nothing(ImmutableList.of, ImmutableSet.of, false)
      val bindings = StreamSupport.stream(values.getBindingSets.spliterator, false).map((bs: BindingSet) => empty.extendWithBindings(StreamSupport.stream(bs.spliterator, false), (be) => ofac.getVariable(be.getName), (be, vars) => getTermForLiteralOrIri(be.getValue))).collect(Collectors.toList)
      val allVars = bindings.stream.flatMap((s) => s.variables.stream).collect(ImmutableCollectors.toSet)
      val res = createFreshNode(allVars)
      bindings.forEach((p) => appendRule(res.atoms.get(0), p.getAtomsExtendedWithNulls(allVars)))
      return res
    }
    else if (node.isInstanceOf[Nothing]) throw new IllegalArgumentException("GROUP BY is not supported yet")
    throw new IllegalArgumentException("Not supported: " + node)
  }

  def dinges(p: ParsedQuery) =
  {
    //p.
    var t = p.getTupleExpr

  }
  @throws[MalformedQueryException]
  def getParsedQuery(sparql: String): ParsedQuery = {
    val parser = QueryParserUtil.createParser(QueryLanguage.SPARQL)
    val pq = parser.parseQuery(sparql, null)
    pq
  }
}

object y
{
  val f = "data/odwn.sprql"
  def munch(f: String):String = scala.io.Source.fromFile(f).getLines.reduceLeft(_+_)
  val t = new SparqlToDatalog
  def main(args: Array[String]) =
  {
    val q = """select ?s ?c where {?s a ?c . values ?c {"aap" "noot"} . ?c a ?x}"""
    val d = t.s2d(munch(f))
    println(d)
    println(Program(d))
  }
}
