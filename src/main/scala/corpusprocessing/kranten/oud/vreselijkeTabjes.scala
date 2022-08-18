package corpusprocessing.kranten.oud
import scala.xml._
object vreselijkeTabjes {
   def processTabjes (article: String) = {
     val a1 = article.replaceAll("&c", "&amp;c")
       .replaceAll("&amp;","&")
       .replaceAll("&","&amp;")
       .replaceAll("<br>", "<lb/>")
     val a2 = a1.split("\n").map(l => if (l.contains("{tab}")) {
       l.replaceAll("\\{tab\\}", "&")
     } else {

     })
   }
}
