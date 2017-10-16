
//import scala.collection.GenTraversableOnce

class XPathTemplate(pathExpressions: Set[String])
{

}

case class BasicPattern(pathExpressions: Set[String], variables: Set[String])
{
  def join(b: BasicPattern) = BasicPattern(this.pathExpressions ++ b.pathExpressions,
    this.variables ++ b.variables)

  def union(b: BasicPattern) =
  {
    val commonVariables = this.variables.intersect(b.variables)
  }

  lazy val pathMap:Map[String,String] = pathExpressions.map(p =>
  {
    val cols = p.split("←")
    val (varName, path) = (cols(0), cols(1))
    varName -> path
  }).toMap

  def varsIn(x: String)  = variables.filter(v => x.contains("$" + v))
  def dep(v: String, p:String):List[(String,String)] = varsIn(p).map(v1 => (v,v1)).toList


  lazy val dependencies = pathMap.map({case (v,p) => dep(v,p)}).flatten
  //lazy val dependencies = dep0.map


  lazy val variablesSorted = TopologicalSort.tsort(dependencies).toList

  def toQuery():String =
  {
    val dollar = "$"
    val fors:String = variablesSorted.reverse.map(v => s"$dollar$v in ${pathMap(v)} ").mkString(",\n")
    val re = variables.map(v => s"<var><name>$v</name><value>$dollar$v</value></var>").mkString("\n")
    s"for  $fors \n return( <result>$re</result>) "
   //  + "return () "
  }
  //def union
}


case class BasicMapping(property: String, subjectName: String, objectName: String)
{

}

class XQuery {

}

object x
{
  val t1 = BasicPattern(Set(
    "x←$y//entry/@id",
    "y←$u//entry",
    "f←$y//form[@type='lemma']",
    "u←document('data/wnt.xml')",
    "z←$y::parent"
  ), Set("u", "x", "y", "z","f"))


  def main(args: Array[String]) =
  {
    //val a = 1
    println(t1.toQuery)
    //println(t1.pathExpressions.map(p => t1.varsIn(p)))
  }
}



