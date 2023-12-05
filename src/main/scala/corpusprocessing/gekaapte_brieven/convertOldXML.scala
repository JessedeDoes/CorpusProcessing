package corpusprocessing.gekaapte_brieven

import scala.xml._
import utils.PostProcessXML._

object convertOldXML {
   def adapt(s: String)  = s.replaceAll("\\[([^\\]\\[]*)\\]", "<note resp='transcriber'>$1</note>").replaceAll("\\n", "<lb/>\n")
   def convertOldXML(xml: String)  = {

     val parsed = XML.loadString(xml)
     val text = (parsed \\ "Transcription"  \ "Text").toString()
     val atext = adapt(text)
     val divje = XML.loadString(atext).copy(label="div")

     val looksEmpty = (divje \\ "lb").isEmpty && divje.text.toLowerCase.contains("excel")
     // print(divje)
     if (looksEmpty)
     {
       Console.err.println(s"Looks empty: $divje")
       <div><note resp="transcriber">{divje.child}</note></div>
     }
     else divje
   }
}
