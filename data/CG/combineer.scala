object o
{
 
  def main(args: Array[String]) = 
  { 
    val crmTags = scala.io.Source.fromFile("crm.log").getLines.map(x => x.split("\\s+").toList).map(x => x(0) -> x(1)).toMap

    val cgTagsPlus = scala.io.Source.fromFile("crm.log").getLines.map(x => x.split("\\s+").toList).map(x => x(0) -> x(2)).toMap
    val cgTags =  scala.io.Source.fromFile("hevel.log").getLines.map(x => x.split("\\s+").toList).map(x => x(0) -> x(1)).toMap

    val cg = cgTagsPlus ++ cgTags

    val morfcodes = scala.io.Source.fromFile("morfcodes.txt").getLines.map(x => x.split("\\t").toList)
       .map(x => "%03d".format(x(0).toInt) -> x(1)).toMap

    val allKeys = (crmTags.keySet ++ cg.keySet ++ morfcodes.keySet).toList.sorted

    allKeys.foreach( k => println(s"$k\t${cg.getOrElse(k,"_")}\t${crmTags.getOrElse(k,"_")}\t${morfcodes.getOrElse(k,"_")}")   ) 
 }
}
