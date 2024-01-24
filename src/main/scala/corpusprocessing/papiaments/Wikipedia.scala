package corpusprocessing.papiaments
import scala.xml._
import sys.process._
import scala.util.{Try,Success,Failure}
import java.io.PrintWriter

object Wikipedia {
   val dump_file="/mnt/Projecten/Taalbank/Papiaments/Corpusdata/Papwiki/papwiki-20240120-pages-articles-multistream.xml"
   val separate_texts = "/mnt/Projecten/Taalbank/Papiaments/Corpusdata/Papwiki/Separate/"
   lazy val doc = XML.load(dump_file)

   lazy val texts = (doc \\ "text").map(_.text)
   lazy val pages = (doc \\ "page")

   def dumpPage(p: Node) = {
     val title = (p \ "title").text
     println(title)
     val id = (p \ "id").text
     val fileName = s"${id}_$title.wiki".replaceAll("/","_")
     val f = separate_texts + fileName
     val text = (p \\ "text").text
     val pw = new PrintWriter(f)
     pw.print(text)
     pw.close()
     val command = Seq("pandoc", "--from", "mediawiki", "--to", "tei", "--no-highlight", f)
     val lines: List[String] = Try((command lineStream).toList) match {
       case Success(value) => value
       case _ => List()
     }
     if (lines.length > 10 && !(title.contains(":") || title.contains("/"))) {
       val xml = "<TEI><text>" + lines.mkString("\n")  + "</text></TEI>"
       val TEI = separate_texts + "TEI/" + fileName.replaceAll(".wiki$", ".tei.xml")
       println(s"Converting to $TEI with pandoc")
       val pw = new PrintWriter(TEI)
       pw.println(xml)
       pw.close()
     }
   }
   def main(args: Array[String])  = {
     pages.foreach(dumpPage)
   }
}
