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
import it.unibz.inf.ontop.owlrefplatform.core.abox.SemanticIndexURIMap
import it.unibz.inf.ontop.owlrefplatform.core.basicoperations.UriTemplateMatcher
import it.unibz.inf.ontop.owlrefplatform.core.basicoperations.VocabularyValidator
import it.unibz.inf.ontop.owlrefplatform.core.translator.DatalogToSparqlTranslator
import it.unibz.inf.ontop.owlrefplatform.core.translator.SPARQLQueryFlattener
import it.unibz.inf.ontop.owlrefplatform.core.translator.SparqlAlgebraToDatalogTranslator


class XPathTemplate(f: String=>String)
{
  val pz = Set(
    "$x←$y//entry/@id",
    "$y←//entry",
    "$z←$y::parent"
  )
}

case class BasicPattern(pathExpressions: Set[String])
{
  def join(b: BasicPattern) = BasicPattern(this.pathExpressions ++ b.pathExpressions)
}

case class BasicMapping(property: String, subjectName: String, objectName: String)
{

}

class XQuery {

}

object x
{
  def main(args: Array[String]) =
  {
    val a = 1

  }
}



