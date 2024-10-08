package servlet

import database.{Configuration, Database, Results2Json}
import javax.servlet.http.HttpServlet
import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse

import scala.collection.JavaConversions._

class Results2JsonServlet(db: Database) extends HttpServlet {

  override def doGet(request: HttpServletRequest, response: HttpServletResponse) {

    response.setContentType("application/json")
    response.setCharacterEncoding("UTF-8")
    response.addHeader("Access-Control-Allow-Origin", "*")

    request.getHeaderNames.foreach(n => { System.err.println(n + " -->  " + request.getHeader(n)) })

    val query = request.getParameter("query")

    if (query != null)
    {
      System.err.println(query)
      val r = Results2Json.resultsAsJsonString(db, query, Some("gis"))
      response.getWriter.write(r)
    }
  }

  override def doPost(request: HttpServletRequest, response: HttpServletResponse)  = doGet(request, response)
}

object Stuff
{
  val testje = Configuration(name="oefen", server="localhost", database="oefen", user="postgres", password="postgres")
  val dsdd = testje.copy(server="dsdd", database="hercules",password="inl")
  val testDB = new Database(dsdd)
}

class DSDDProxy extends Results2JsonServlet(Stuff.testDB)




