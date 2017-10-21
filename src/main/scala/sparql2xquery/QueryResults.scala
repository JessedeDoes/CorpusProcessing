package sparql2xquery
import org.json4s._
import org.json4s.native.Serialization
import org.json4s.native.Serialization.{read, write}


object QueryResults {

  //case class Vars(vars: List[String])
  //case class Results(val bindings: Map[String, Value])

  case class Head(val vars: List[String])
  case class Value(`type`: String, value: String)
  case class Results(bindings: List[Map[String,Value]])

  implicit val formats = Serialization.formats(NoTypeHints)

  case class Response(head: Head, results: Results)
  {
    def toJSON():String = write(this)
  }

  def toJSON(results: Seq[Map[String,scala.xml.Node]]): String =
  {
     val vars = results.flatMap(_.keySet).distinct.toList
     val head = Head(vars)
     val bindings = results.map(
       m => m.map( { case (k,v) => (k -> Value("literal", v.toString ) )  }).toMap
     ).toList
     val r = Results(bindings)
     val response = Response(head, r)
     return response.toJSON()
  }

  def main(args: Array[String]) =
  {
    val x = List(1,2,3)
  }
}
