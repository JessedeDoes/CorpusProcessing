package servlet

import javax.servlet.http.HttpServlet
import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse
import sparql2xquery._

// https://github.com/earldouglas/xsbt-web-plugin/blob/master/docs/4.0.x.md

// bleuh volgens mij verwacht jena rdf/xml bij federated query...
import scala.collection.JavaConversions._

class BogusEndpoint extends HttpServlet
{
  override def doGet(request: HttpServletRequest, response: HttpServletResponse) {

    response.setContentType("application/sparql-results+json")
    response.setCharacterEncoding("UTF-8")
    //response.getWriter.write("""<h1>Hello, world!</h1>""")
    request.getHeaderNames.foreach(
      n => { System.err.println(n + " -->  " + request.getHeader(n)) }
    )
    val query = request.getParameter("query")
    if (query != null)
      {
        System.err.println(query)
        val results = SparqlToXquery.translateAndRunQuery(query)
        System.err.println(results)
        response.getWriter.write(results)
      }
  }
}


// http://svprre02:8080/fuseki/tdb/sparql?query=SELECT++*%0AWHERE%0A++%7B+%3Fs++%3Fp++%22dom%22+%7D%0A
// https://www.w3.org/TR/sparql11-results-json/
// response structure (JSON-ld):

/*

 HTTP/1.1 200 OK
  Server: Apache-Coyote/1.1
  Fuseki-Request-ID: 19
  Vary: Accept,Accept-Encoding,Accept-Charset
  Cache-Control: must-revalidate,no-cache,no-store
  Pragma: no-cache
  Content-Type: application/sparql-results+json;charset=utf-8
  Transfer-Encoding: chunked
  Date: Sat, 21 Oct 2017 12:34:46 GMT

{
  "head": {
    "vars": [ "s" , "p" ]
  } ,
  "results": {
    "bindings": [
      {
        "s": { "type": "uri" , "value": "http://www.ivdnt.org/diamant#gek.dom" } ,
        "p": { "type": "uri" , "value": "http://www.ivdnt.org/diamant#target" }
      } ,
 */