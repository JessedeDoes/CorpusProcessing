package corpusprocessing.papiaments.woordenboeken
import scala.xml._
object jansen {


   case class Sense(s: String, entry_id: Int) {
     lazy val (number, text) = if (s.matches("[0-9]\\..*")) {
       s.replaceAll("([0-9]\\.)\\s*(.*)", "$1").replaceAll("\\.", "") ->  s.replaceAll("([0-9]\\.)\\s*(.*)", "$2")
     } else ("1", s)

     lazy val (label,translation) = if (text.matches(".*\\(.*\\).*"))
       text.replaceAll(".*(\\(.*\\)).*","$1") ->  text.replaceAll(".*(\\(.*\\))\\s*(.*)","$2")
     else "" -> text
     override def toString = s"[$number] $translation [$label]"
     def toTSV = s"$entry_id\t$number\t$translation\t$label"
   }
   case class Entry(n: Node, i:Int) {
     val dutch = (n \ "hi").text
     val txt = n.child.filter(_.isInstanceOf[Text]).map(_.text).filter(_.trim.nonEmpty).mkString(" ").replaceAll("^;\\s*", "").replaceAll("\\.\\s*$", "")
     val preSenses: Seq[String] = (if (txt.contains("1.")) {
       txt.replaceAll("([0-9]\\.)", "<sense>$1").split("<sense>").filter(_.nonEmpty).toList
     } else Seq(txt))

     lazy val senses = preSenses.map(x => Sense(x,i))
     override def toString =
       s"""e$i: $dutch
          |${senses.map(s => "\t" + s.toString).mkString("\n")}""".stripMargin
     def toTSV = s"$i\t$dutch\t${senses.map(_.toString).mkString("; ")}"
   }
   lazy val jansen = XML.load("/mnt/Projecten/Papiaments/Woordenboeken/OudereWdbVanSevering/DBNLxml/jans550nede01_01.xml")
   lazy val entries = (jansen \\ "item").zipWithIndex.map({case (e,i) => Entry(e,i)})

   def main(args:Array[String])  = {

     import java.io.PrintWriter
     val entryWriter = new PrintWriter("/tmp/jansen.entries.txt")
     val senseWriter = new PrintWriter("/tmp/jansen.senses.txt")
     entries.foreach(e =>  {
       entryWriter.println(e.toTSV)
       e.senses.foreach(s => senseWriter.println(s.toTSV))
       println()
       //println(e.n)
       println(e.toString) }
     )
   }
}
