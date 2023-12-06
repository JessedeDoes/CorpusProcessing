package corpusprocessing.gekaapte_brieven
import zeeuws.HTML

import java.io.PrintWriter
object NationalArchive {

   val max = 15
   def getInfo(url: String)  = {
     val content = io.Source.fromURL(url).getLines().mkString("\n")
     val doc = HTML.parse(content)
     val table = (doc \\ "table").filter(x => (x \ "@class").text.contains("asset-details"))
     val caption = (table \\ "caption").text.trim
     val fields = (table \\ "tr").map(row => {
       val name = (row \ "th").text
       val value = (row \ "td").text
       name.trim.replaceAll(":$", "").toLowerCase().replaceAll("[^a-z]", "_") -> value.trim.replaceAll("\n", "<br/>")
     })
     (fields :+ ("caption" -> caption)).toMap
   }

   def downloadInfo()  = {
     exportCorpus.init()
     val urlMapping = exportCorpus.articlesUnfiltered.map(a => {
       val url = (a -> "originele_vindplaats_xln").replaceAll("^http", "https")
       (a -> "brief_id") -> url
     }).toMap

     val urls = urlMapping.values.toSet
     val infos = urls.map(url => {
       println(s"##### $url #####")
       val info = getInfo(url)
       info.foreach(println)
       url -> (info ++ Map("url" -> url))
     }).toMap

     val infosPlus = exportCorpus.articlesUnfiltered.map(a => {
       val url = (a -> "originele_vindplaats_xln").replaceAll("^http", "https")
       val info = infos(url) ++ Map("brief_id"-> a.id)
       info
     })
     val fields = infosPlus.flatMap(i => i.keySet).toSet.toList.sorted
     val pw = new PrintWriter("/tmp/archive_info.txt")
     val header = fields.mkString("\t")
     pw.println(header)
     infosPlus.foreach(i => {
        val values = fields.map(f => i.getOrElse(f, "")).mkString("\t")
        pw.println(values)
     })
     pw.close()
   }

  def main(args: Array[String])  = {
    downloadInfo()
  }
}
