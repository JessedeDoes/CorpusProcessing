
trait XQueryNode
{
  def toQuery(): String
}


object XQueryNode
{
  type Variable = String
}

import XQueryNode.Variable


case class ValueRestrictionSet(m: Map[Variable,List[String]]) extends XQueryNode
{
  val dollar = "$"
  def toQuery():String =
  {
     val conditions = m.keySet.map(
       v => {
         val cond = s"(${m(v).mkString(",")}) = $dollar${v}"
         cond
       }
     )
    s"where ${conditions.mkString(" and ")}"
  }

  def isEmpty():Boolean = m.isEmpty

  def ++(b: ValueRestrictionSet) =
    ValueRestrictionSet(this.m ++ b.m)
}

object ValueRestrictionSet
{
  val empty = ValueRestrictionSet(Map.empty)
}


trait XQuerySelect extends XQueryNode
{
  val pathExpressions: Set[String]
  def valueRestrictions: ValueRestrictionSet=ValueRestrictionSet.empty
  val variables: Set[Variable]

  def join(b: BasicPattern) = BasicPattern(this.pathExpressions ++ b.pathExpressions,
    this.variables ++ b.variables)

  def isSelected(v:String) = true

  def union(b: BasicPattern) =
  {
    val commonVariables = this.variables.intersect(b.variables)
  }

  lazy val pathMap:Map[Variable,String] = pathExpressions.map(p =>
  {
    val cols = p.split("\\s*←\\s*")
    val (varName, path) = (cols(0), cols(1))
    varName -> path
  }).toMap

  def varsIn(x: String)  = variables.filter(v => x.contains("$" + v))

  def dep(v: Variable, p:String):List[(String,String)] = varsIn(p).map(v1 => (v,v1)).toList

  lazy val dependencies = pathMap.map({case (v,p) => dep(v,p)}).flatten

  lazy val variablesSorted = TopologicalSort.tsort(dependencies).toList.reverse

  //ToDo: Maak niet geselecteerde variabelen verborgen (hernoem naar iets globaal unieks)

  def toQuery():String =
  {
    val dollar = "$"
    println("restrictions: " + valueRestrictions)
    val forPart:String = variablesSorted.map(v => s"$dollar$v in ${pathMap(v)} ").mkString(",\n")
    val returnPart = variables.filter(isSelected).map(v => s"<var><name>$v</name><value>{$dollar$v}</value></var>").mkString("\n")
    val wherePart = if (valueRestrictions.isEmpty) "" else valueRestrictions.toQuery()
    s"for  $forPart \n $wherePart \n return( <result>$returnPart</result>) "
  }

  def toPath(): String =
  {
    def foldie(m:Map[String, String], v:String):Map[String,String] =
      m.map(
        { case (v1,p) =>
          println(v + " in " + p)
          (v1, p.replaceAll("\\$" + v, m(v)) )}
      )
    val mx = variablesSorted.foldLeft(pathMap)(foldie)
    mx.toString
  }
}

case class BasicPattern(pathExpressions: Set[String], variables: Set[Variable]=Set("subject", "object")) extends XQuerySelect
{
  val dollar = "$"

  def renameVars(mapping: Map[String,String]) =
  {
    val newVars = variables.map(v => mapping.getOrElse(v,v))

    def replaceOneVar(p: String, before: String, after: String) =
      p.replaceAll(s"\\s*$before\\s*←", s"$after←")
      .replaceAll("\\" + dollar + before, "\\"  + dollar + after)

    def replaceOne(p: String, kv: (String,String)): String =
    {
      val (a, b) = kv;
      replaceOneVar(p,a,b)
    }

    def replaceAllVars(s:String) = mapping.foldLeft(s)(replaceOne)

    BasicPattern(pathExpressions.map(replaceAllVars), newVars)
  }
  // def union
}



case class SimpleSelect(pathExpressions: Set[String],
                        variables: Set[Variable],
                        selected: Set[Variable],
                        override val valueRestrictions: ValueRestrictionSet=ValueRestrictionSet.empty)
  extends XQuerySelect
{
  override def isSelected(v: String) = selected.contains(v)
}


class XQuery {

}

object x
{
  val t1 = BasicPattern(Set(
    "x←$y//sense/[@ref=$y/@id]",
    "y←$u//entry",
    "f←$y//form[@type='lemma']",
    "u←doc('data/wnt.xml')",
    "z←$y/.."
  ), Set("u", "x", "y", "z","f"))


  val t2 = SimpleSelect(Set(
    "s←//node[@cat='smain']",
    "subj←$s/node[@rel='su']",
    "obj←$s/node[@rel='obj1']"
  ), Set("s", "subj", "obj"),
    Set("subj", "obj")
  )
  def main(args: Array[String]) =
  {
    println(t2.toQuery)
    //println(t1.toPath)
  }
}


/**
  * Bij
  * x=//entry
  * y=$x/@id[0]
  * z=$x//sense[@id=$y]
  *
  */
