import scala.xml._

object o
{
  def linkje(p: String) = <a href={s"http://pcob67.inl.loc:8080/corpus-frontend/TestCRM/search?page=0&pageSize=_20&pattern=((name~poscode.case~false.value~_$p))&operation=hits"}>{p}</a>

  def main(args: Array[String]) = 
  { 
    val crmTags = scala.io.Source.fromFile("crm.log").getLines.map(x => x.split("\\s+").toList).map(x => x(0) -> x(1)).toMap

    val cgTagsPlus = scala.io.Source.fromFile("crm.log").getLines.map(x => x.split("\\s+").toList).map(x => x(0) -> x(2)).toMap
    val cgTags =  scala.io.Source.fromFile("hevel.log").getLines.map(x => x.split("\\s+").toList).map(x => x(0) -> x(1)).toMap

    val cg = cgTagsPlus ++ cgTags

    val morfcodes = scala.io.Source.fromFile("morfcodes.txt").getLines.map(x => x.split("\\t").toList)
       .map(x => "%03d".format(x(0).toInt) -> x(1)).toMap

    val allKeys = (crmTags.keySet ++ cg.keySet ++ morfcodes.keySet).toList.sorted

//    allKeys.foreach( k => println(s"$k\t${cg.getOrElse(k,"_")}\t${crmTags.getOrElse(k,"_")}\t${morfcodes.getOrElse(k,"_")}")   ) 
    
    val xml = <table>
              {allKeys.map(k =>
                <tr>
                  <td>
                     {linkje(k)}
                  </td>
                   <td>
                       {cg.getOrElse(k,"_")}
                   </td>
                   <td>
                        {crmTags.getOrElse(k,"_")}
                    </td>
                    <td>
                        {morfcodes.getOrElse(k,"_")}
                    </td>
                  </tr>)
               }
             </table>
    

             println(xml)
 }
}
