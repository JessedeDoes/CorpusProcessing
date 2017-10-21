package sparql2xquery

trait XQueryNode
{
  def toQuery(): String
}

object XQueryNode
{
  type Variable = String
  var varNumber = 0

  def getNewVarName():String = { varNumber += 1; "var" + varNumber}

}