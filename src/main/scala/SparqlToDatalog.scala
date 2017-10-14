import org.openrdf.query.{BindingSet, MalformedQueryException, QueryLanguage}
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

import com.google.common.collect.ImmutableList
import com.google.common.collect.ImmutableSet
import it.unibz.inf.ontop.model.Predicate.COL_TYPE
import it.unibz.inf.ontop.model.Term
import it.unibz.inf.ontop.model.impl.OBDAVocabulary
import org.openrdf.query.algebra.StatementPattern
import it.unibz.inf.ontop.model.Predicate
// kijk naar: ../ontop/reformulation-core/src/main/java/it/unibz/inf/ontop/owlrefplatform/core/translator/SparqlAlgebraToDatalogTranslator.java

import com.google.common.collect.ImmutableSet
import it.unibz.inf.ontop.model.Predicate.COL_TYPE
import it.unibz.inf.ontop.model.Term
import org.openrdf.model.datatypes.XMLDatatypeUtil
import org.openrdf.query.algebra.Var


import it.unibz.inf.ontop.model.Predicate.COL_TYPE
import it.unibz.inf.ontop.model.Term
import it.unibz.inf.ontop.parser.EncodeForURI


import com.google.common.collect.ImmutableList
import com.google.common.collect.ImmutableSet
import java.util

class SparqlToDatalog
{
  import org.openrdf.query.parser.ParsedQuery
  val uriTemplateMatcher = new UriTemplateMatcher
  val translator = new SparqlAlgebraToDatalogTranslator(new UriTemplateMatcher, new SemanticIndexURIMap)
  var predicateIdx:Int = 0

  import it.unibz.inf.ontop.owlrefplatform.core.abox.SemanticIndexURIMap

  private val uriRef:SemanticIndexURIMap = null
  import it.unibz.inf.ontop.owlrefplatform.core.basicoperations.UriTemplateMatcher

  //private val uriTemplateMatcher = translator
  @throws[Exception]
  def s2d(sparql: String): DatalogProgram =
  {
    val pq = getParsedQuery(sparql)
    translator.translate(pq).getProgram // let op signature is ook interessant
  }

  import it.unibz.inf.ontop.model.OBDADataFactory
  import it.unibz.inf.ontop.model.impl.OBDADataFactoryImpl

  private val ofac = OBDADataFactoryImpl.getInstance

  import it.unibz.inf.ontop.model.DatatypeFactory
  import it.unibz.inf.ontop.model.impl.OBDADataFactoryImpl

  private val dtfac = OBDADataFactoryImpl.getInstance.getDatatypeFactory

  private class TranslationResult(val atoms: ImmutableList[Function],
                                  val variables: ImmutableSet[Variable],
                                  val isBGP: Boolean)
  {
    final val ofac = OBDADataFactoryImpl.getInstance

    /**
      * Extends the current translation result with bindings coming from {@link Extension} (expr AS ?x) or {@link BindingSetAssignment} (VALUES in SPARQL)
      *
      * @param bindings  a stream of bindings. A binding is a pair of a variable, and a value/expression
      * @param varMapper a function from bindings to { @link Variable}s
      * @param exprMapper a function maps a pair of a binding and a set variables to a { @link Term}
      * //@param T       A class for binding. E.g. { @link org.openrdf.query.Binding} or { @link org.openrdf.query.algebra.ExtensionElem}
      * @return extended translation result
      */

    def extendWithBindings[T](bindings: java.util.stream.Stream[T],
                              varMapper: java.util.function.Function[_ >: T, Variable],
                              exprMapper: java.util.function.BiFunction[_ >: T,
                                ImmutableSet[Variable], Term]): TranslationResult =
    {
      val vars = new java.util.HashSet[Variable](variables)
      val eqAtoms:java.util.stream.Stream[Function] = bindings.map((b) => {

          val expr = exprMapper.apply(b, ImmutableSet.copyOf(vars))
          val v = varMapper.apply(b)
          if (!vars.add(v)) throw new IllegalArgumentException("Duplicate binding for variable " + v)
          ofac.getFunctionEQ(v, expr)
      })
      new TranslationResult(getAtomsExtended(eqAtoms), ImmutableSet.copyOf(vars), false)
    }

    def getAtomsExtendedWithNulls(allVariables: ImmutableSet[Nothing]): ImmutableList[Function] = {
      val nullVariables = Sets.difference(allVariables, variables)
      if (nullVariables.isEmpty) return atoms
      getAtomsExtended(nullVariables.stream.map(v => ofac.getFunctionEQ(v, OBDAVocabulary.NULL)))
    }

    /**
      * Extends the atoms in current translation result with {@code extension}
      *
      * @param extension a stream of functions to be added
      * @return extended list of atoms
      */
    def getAtomsExtended(extension: java.util.stream.Stream[Function]): ImmutableList[Function] = {
      val x =  extension.collect(Collectors.toList[Function]())
      ImmutableList.copyOf(Iterables.concat(atoms, x))
      //            ImmutableList.Builder builder = ImmutableList.<Function>builder().addAll(atoms)
      //                    .addAll(nullVariables.stream().map(v -> ofac.getFunctionEQ(v, OBDAVocabulary.NULL)).iterator());
      //            return builder.build();
    }
  }


  private def getTermForVariable(v: Var, variables: ImmutableSet.Builder[Variable]) = {
    val `var` = ofac.getVariable(v.getName)
    variables.add(`var`)
    `var`
  }

  private def getTermForLiteralOrIri(v: Value): Term = {
    if (v.isInstanceOf[Literal]) return getTermForLiteral(v.asInstanceOf[Literal])
    else if (v.isInstanceOf[URI]) return getTermForIri(v.asInstanceOf[URI], false)
    throw new RuntimeException("The value " + v + " is not supported yet!")
  }

  private def getTermForLiteral(literal: Literal): Term = {
    val typeURI = literal.getDatatype
    val value = literal.getLabel
    var datatype:COL_TYPE = null
    if (typeURI == null) datatype = COL_TYPE.LITERAL
    else {
      datatype = dtfac.getDatatype(typeURI)
      if (datatype == null) { // ROMAN (27 June 2016): type1 in open-eq-05 test would not be supported in OWL
        // the actual value is LOST here
        return ofac.getUriTemplateForDatatype(typeURI.stringValue)
      }
      // old strict version:
      // throw new RuntimeException("Unsupported datatype: " + typeURI);
      // check if the value is (lexically) correct for the specified datatype
      if (!XMLDatatypeUtil.isValidValue(value, typeURI)) throw new RuntimeException("Invalid lexical form for datatype. Found: " + value)
    }
    val constant = ofac.getConstantLiteral(value, datatype)
    // wrap the constant in its datatype function
    if (datatype eq COL_TYPE.LITERAL) { // if the object has type LITERAL, check the language tag
      val lang = literal.getLanguage
      if (lang != null && !(lang == "")) return ofac.getTypedTerm(constant, lang)
    }
    ofac.getTypedTerm(constant, datatype)
  }


  private def getTermForIri(v: URI, unknownUrisToTemplates: Boolean):Term = {

    val t = new Term {}

    val uri = EncodeForURI.decodeURIEscapeCodes(v.stringValue)
    if (uriRef != null) { // if in the Semantic Index mode
      val id = uriRef.getId(uri)
      if (id < 0 && unknownUrisToTemplates) { // URI is not found and need to wrap it in a template
        ofac.getUriTemplateForDatatype(uri)
      }
      else ofac.getUriTemplate(ofac.getConstantLiteral(String.valueOf(id), COL_TYPE.INTEGER))
    }
    else {
      var constantFunction = uriTemplateMatcher.generateURIFunction(uri)
      if ((constantFunction.getArity eq 1) && unknownUrisToTemplates) { // ROMAN (27 June 2016: this means ZERO arguments, e.g., xsd:double or :z
        // despite the name, this is NOT necessarily a datatype
        constantFunction = ofac.getUriTemplateForDatatype(uri)
      }
      constantFunction
    }
  }

  private def translateTriplePattern(triple: StatementPattern):TranslationResult =
  { // A triple pattern is member of the set (RDF-T + V) x (I + V) x (RDF-T + V)
    // VarOrTerm ::=  Var | GraphTerm
    // GraphTerm ::=  iri | RDFLiteral | NumericLiteral | BooleanLiteral | BlankNode | NIL
    val variables = ImmutableSet.builder[Variable]()
    var atom:Function = null

    val s:Value = triple.getSubjectVar.getValue
    val p = triple.getPredicateVar.getValue
    val o = triple.getObjectVar.getValue

    val sTerm:Term = if (s == null)
      getTermForVariable(triple.getSubjectVar, variables)
    else
      getTermForLiteralOrIri(s)

    if (p == null) { //  term variable term .
      val pTerm = getTermForVariable(triple.getPredicateVar, variables)
      val oTerm = if (o == null) getTermForVariable(triple.getObjectVar, variables)
      else getTermForLiteralOrIri(o)
      atom = ofac.getTripleAtom(sTerm, pTerm, oTerm)
    } else if (p.isInstanceOf[URI]) if (p.equals(RDF.TYPE)) if (o == null) { // term rdf:type variable .
      val pTerm = ofac.getUriTemplate(ofac.getConstantLiteral(OBDAVocabulary.RDF_TYPE))
      val oTerm = getTermForVariable(triple.getObjectVar, variables)
      atom = ofac.getTripleAtom(sTerm, pTerm, oTerm)
    }
    else if (o.isInstanceOf[URI]) { // term rdf:type uri .
      val datatype = dtfac.getDatatype(o.asInstanceOf[URI])
      if (datatype != null) { // datatype
        atom = ofac.getFunction(dtfac.getTypePredicate(datatype), sTerm)
      }
      else { // class
        atom = ofac.getFunction(ofac.getClassPredicate(o.stringValue), sTerm)
      }
    }
    else throw new IllegalArgumentException("Unsupported query syntax")
    else { // term uri term . (where uri is either an object or a datatype property)
      val oTerm = if (o == null) getTermForVariable(triple.getObjectVar, variables)
      else getTermForLiteralOrIri(o)
      val predicate = ofac.getPredicate(p.stringValue, Array[Predicate.COL_TYPE](null, null))
      atom = ofac.getFunction(predicate, sTerm, oTerm)
    }
    else { // if predicate is a variable or literal
      throw new RuntimeException("Unsupported query syntax")
    }
    new TranslationResult(ImmutableList.of(atom), variables.build, true)
  }

  import it.unibz.inf.ontop.model.CQIE
  import java.util

  private def wrapNonTriplePattern(sub: TranslationResult): Function = {
    if (sub.atoms.size > 1 || sub.atoms.get(0).isAlgebraFunction) {
      val head = getFreshHead(new util.ArrayList[Term](sub.variables))
      appendRule(head, sub.atoms)
      return head
    }
    sub.atoms.get(0)
  }


  private def createFreshNode(vars: ImmutableSet[Variable]) = {
    val head = getFreshHead(new java.util.ArrayList[Term](vars))
    new Nothing(ImmutableList.of(head), vars, false)
  }
  import it.unibz.inf.ontop.model.Term
  import it.unibz.inf.ontop.model.impl.OBDAVocabulary

  private def getFreshHead(terms: java.util.List[Term]):Function = {
    val pred = ofac.getPredicate(OBDAVocabulary.QUEST_QUERY + predicateIdx, terms.size)
    predicateIdx += 1
    ofac.getFunction(pred, terms)
  }

  private def appendRule(head: Function, body: List[Function]): Unit = {
    val rule = ofac.getCQIE(head, body)
    //program.appendRule(rule)
  }

  private def translate(node: TupleExpr): TranslationResult = { //System.out.println("node: \n" + node);
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
      return new TranslationResult(ImmutableList.of(), ImmutableSet.of(), true)
    }
    else if (node.isInstanceOf[Join]) { // JOIN algebra operation
      val join = node.asInstanceOf[Join]
      val a1 = translate(join.getLeftArg)
      val a2 = translate(join.getRightArg)
      val vars = Sets.union(a1.variables, a2.variables).immutableCopy
      if (a1.isBGP && a2.isBGP) { // collect triple patterns into BGPs
        val atoms = ImmutableList.builder[Function].addAll(a1.atoms).addAll(a2.atoms).build
        return new TranslationResult(atoms, vars, true)
      }
      else {
        val body = ofac.getSPARQLJoin(wrapNonTriplePattern(a1), wrapNonTriplePattern(a2))
        return new TranslationResult(ImmutableList.of(body), vars, false)
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
      return new TranslationResult(ImmutableList.of(body), vars, false)
    }
    else if (node.isInstanceOf[Union]) { // UNION algebra operation
      val union = node.asInstanceOf[Union]
      val a1 = translate(union.getLeftArg)
      val a2 = translate(union.getRightArg)
      val vars = Sets.union(a1.variables, a2.variables).immutableCopy
      val res = createFreshNode(vars)
      appendRule(res.atoms.get(0), a1.getAtomsExtendedWithNulls(vars))
      appendRule(res.atoms.get(0), a2.getAtomsExtendedWithNulls(vars))
      return res
    }
    else if (node.isInstanceOf[Filter]) { // FILTER algebra operation
      val filter = node.asInstanceOf[Filter]
      val a = translate(filter.getArg)
      val f = getFilterExpression(filter.getCondition, a.variables)
      val atoms = ImmutableList.builder[Function].addAll(a.atoms).add(f).build
      // TODO: split ANDs in the FILTER?
      return new Nothing(atoms, a.variables, false)
    }
    else if (node.isInstanceOf[Projection]) { // PROJECT algebra operation
      val projection = node.asInstanceOf[Projection]
      val sub = translate(projection.getArg)
      val pes = projection.getProjectionElemList.getElements
      // the two lists are required to synchronise the order of variables
      val sVars = new java.util.ArrayList[Term](pes.size)
      val tVars = new java.util.ArrayList[Term](pes.size)
      var noRenaming = true
      import scala.collection.JavaConversions._
      for (pe <- pes) {
        val sVar = ofac.getVariable(pe.getSourceName)
        if (!sub.variables.contains(sVar))
          throw new IllegalArgumentException("Projection source of " + pe + " not found in " + projection.getArg)
        sVars.add(sVar)
        val tVar = ofac.getVariable(pe.getTargetName)
        tVars.add(tVar)
        if (!sVar.equals(tVar)) noRenaming = false
      }
      if (noRenaming && sVars.containsAll(sub.variables)) { // neither projection nor renaming
        return sub
      }
      val v0 = tVars.stream.map((t) => t.asInstanceOf[Variable])
      val set = v0.collect(Collectors.toSet[Variable]())
      val vars = ImmutableSet.copyOf(set)
      if (noRenaming) return new TranslationResult(sub.atoms, vars, false)
      val head = getFreshHead(sVars)
      appendRule(head, sub.atoms)
      val atom = ofac.getFunction(head.getFunctionSymbol, tVars)
      return new TranslationResult(ImmutableList.of(atom), vars, false)
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
      val empty = new TranslationResult(ImmutableList.of(), ImmutableSet.of(), false)
      val bindings = StreamSupport.stream(values.getBindingSets.spliterator, false)
        .map((bs: BindingSet) => empty.extendWithBindings(StreamSupport.stream(bs.spliterator, false),
          be => ofac.getVariable(be.getName),
          (be, vars) => getTermForLiteralOrIri(be.getValue))).collect(Collectors.toList)
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
