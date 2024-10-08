package corpusprocessing.papiaments.woordenboeken

import utils.HTML

import java.io.{File, PrintWriter}
import scala.xml._
object Schooltaalwoorden {

  def parseSchoolHTML(schoolHTML: String) = {
    val out = new PrintWriter("/tmp/salto.tsv")
    lazy val schoolXML = HTML.parse(new File(schoolHTML))
    val z = (schoolXML \\ "tr").map(x => {

      val cells = (x \\ "td" ++ x \\ "th")
      val label = cells.head.label
      val n = cells.filter(c => c.text.trim.nonEmpty).size
      if (n > 0)
         out.println((Seq(label) ++ cells.map(_.text.trim)).mkString("\t"))
      else println(cells.map(_.text.trim))
      n
    }).toSet
    println(z)
    out.close()
  }

  def main(args: Array[String]): Unit  = {
    parseSchoolHTML(args.headOption.getOrElse("data/salto.html"))
  }
}
