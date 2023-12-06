package corpusprocessing.gekaapte_brieven

import scala.xml._
import utils.PostProcessXML._

object convertOldXML {
   def adapt(s: String)  = s.replaceAll("\\[([^\\]\\[]*)\\]", "<note resp='transcriber'>$1</note>").replaceAll("\\n", "<lb/>\n")
   def convertOldXML(xml: String)  = {

     val parsed = XML.loadString(xml)

     // dit werkt niet voor de geconverteerde xml
     val oldTextElement = (parsed \\ "Transcription"  \ "Text")
     val text = if (oldTextElement.isEmpty) xml else adapt(oldTextElement.toString())

     //val atext = adapt(text)

     val divje = XML.loadString(text).copy(label="div")

     val looksEmpty = (divje \\ "lb" ++ divje \\ "cell").isEmpty && divje.text.toLowerCase.contains("excel")
     // print(divje)
     if (looksEmpty)
     {
       Console.err.println(s"Looks empty: $divje")
       <div><note resp="transcriber">{divje.child}</note></div>
     }
     else divje
   }
}
