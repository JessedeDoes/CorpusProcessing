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
import scala.collection.JavaConversions._

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


  def dinges(p: ParsedQuery) =
  {
    //p.
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
