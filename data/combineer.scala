object o
{
 
  def main(args: Array[String]) = 
  { 
    val crmTags = scala.io.Source.fromFile("crm.log").getLines.map(x => x.split("\\s+").toList).map(x => x(0) -> x(1)).toMap
    val cgTags =  scala.io.Source.fromFile("hevel.log").getLines.map(x => x.split("\\s+").toList).map(x => x(0) -> x(1)).toMap

    val allKeys = (crmTags.keySet ++ cgTags.keySet).toList.sorted

    allKeys.foreach( k => println(s"$k\t${cgTags.getOrElse(k,"_")}\t${crmTags.getOrElse(k,"_")}")   ) 
 }
}
