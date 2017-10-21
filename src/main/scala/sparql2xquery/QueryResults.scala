package sparql2xquery
import org.json4s._
import org.json4s.native.Serialization
import org.json4s.native.Serialization.{read, write}
import scala.xml._

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
    def toXML() =
      <sparql xmlns="http://www.w3.org/2005/sparql-results#">
        <head>
          {this.head.vars.map(v => <variable name={v}/>)}
        </head>
        <results>
          {this.results.bindings.map(b =>
            <result>
              {b.map( {case (n,v) =>
                <binding name={n}>
                  <literal datatype="http://www.w3.org/2001/XMLSchema#string">{v.value}</literal>
                </binding>
            }  )}
            </result>
          )}
        </results>
      </sparql>

    override def toString():String = toXML().toString()
  }


  def response(results: Seq[Map[String,scala.xml.Node]]): Response =
  {
     val vars = results.flatMap(_.keySet).distinct.toList
     val head = Head(vars)
     val bindings = results.map(
       m => m.map( { case (k,v) => (k -> Value("literal", v.text ) )  }).toMap
     ).toList
     val r = Results(bindings)
     val response = Response(head, r)
     response
     //return response.toJSON()
  }



  def main(args: Array[String]) =
  {
    val x = List(1,2,3)
  }
}

/*
<?xml version="1.0"?>
<sparql xmlns="http://www.w3.org/2005/sparql-results#">

  <head>
    <variable name="x"/>
    <variable name="hpage"/>
    <variable name="name"/>
    <variable name="age"/>
    <variable name="mbox"/>
    <variable name="friend"/>
  </head>

  <results>

    <result>
      <binding name="x">
	<bnode>r2</bnode>
      </binding>
      <binding name="hpage">
	<uri>http://work.example.org/bob/</uri>
      </binding>
      <binding name="name">
	<literal xml:lang="en">Bob</literal>
      </binding>
      <binding name="age">
	<literal datatype="http://www.w3.org/2001/XMLSchema#integer">30</literal>
      </binding>
      <binding name="mbox">
	<uri>mailto:bob@work.example.org</uri>
      </binding>
    </result>

    ...
  </results>

</sparql>
 */