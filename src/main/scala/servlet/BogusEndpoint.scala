package servlet

import javax.servlet.http.HttpServlet
import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse

class BogusEndpoint {

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