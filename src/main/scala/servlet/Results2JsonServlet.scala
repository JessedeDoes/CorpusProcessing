package servlet

import database.{Configuration, Database, Results2Json}
import javax.servlet.http.HttpServlet
import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse
import sparql2xquery._

// https://github.com/earldouglas/xsbt-web-plugin/blob/master/docs/4.0.x.md

// bleuh volgens mij verwacht jena rdf/xml bij federated query...
import scala.collection.JavaConversions._

class Results2JsonServlet(db: Database) extends HttpServlet {
  override def doGet(request: HttpServletRequest, response: HttpServletResponse) {

    response.setContentType("application/json")
    //response.setContentType("application/sparql-results+xml")
    response.setCharacterEncoding("UTF-8")
    //response.getWriter.write("""<h1>Hello, world!</h1>""")
    request.getHeaderNames.foreach(
      n => { System.err.println(n + " -->  " + request.getHeader(n)) }
    )
    val query = request.getParameter("query")
    if (query != null)
    {
      System.err.println(query)
      val r = Results2Json.resultsAsJsonString(db, query)
      response.getWriter.write(r)
    }
  }
}

object Stuff
{
  val testje = Configuration(name="oefen", server="localhost", database="oefen", user="postgres", password="postgres")
  val testDB = new Database(testje)
}

class DSDDProxy extends Results2JsonServlet(Stuff.testDB)



