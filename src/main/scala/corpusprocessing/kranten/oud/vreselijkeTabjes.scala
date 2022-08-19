package corpusprocessing.kranten.oud
import corpusprocessing.clariah_training_corpora.moderne_tagging.grouping.groupWithFirst

import scala.xml._
object vreselijkeTabjes {

   def processTabjes (article: String) = {
     val a1 = article.replaceAll("&c", "&amp;c")
       .replaceAll("&amp;","&")
       .replaceAll("&","&amp;")
       .replaceAll("<br>", "<lb/>")

     val lines: Array[String] = a1.split("\n").map(l => if (l.contains("{tab}")) {
       l.replaceAll("\\{tab\\}", "<tab/>") + "<lb/>"
     } else {
        l
     })

     if (lines.exists(_.contains("<tab/>"))) {
       val groups: Seq[Seq[String]] = groupWithFirst[String](lines, l => !l.contains("<tab"))

       val mappedGroups: Seq[String] = groups.flatMap(g => {
         val tabless = g.filter(!_.contains("<tab/>")).mkString("\n")
         val tabbed: Seq[String] = g.filter(_.contains("<tab/>"))
           .map(x => s"<row>${x.split("<tab/>")
             .map(c => s"<cell>${c.replaceAll("<tab/>", "")}</cell>")
             .mkString("")}</row>")
         if (tabbed.nonEmpty ) List(tabless,  s"<table>${tabbed.mkString("\n")}</table>") else List(tabless)
       })

       mappedGroups.mkString("\n")
     } else lines.mkString("\n")
   }
}
