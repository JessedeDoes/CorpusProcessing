package corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.syntax_poc.spod
import scala.xml._
import java.io.{File, PrintWriter}
import java.net.{URLDecoder, URLEncoder}
object spod {
   val dir = "data/SPOD/"

  val rubrieken = List("Attributen" -> (6, 8),
    "Hoofdzinnen" -> (9, 12),
    "Bijzinnen" -> (13, 29),
    "Correlatieve comparatieven" -> (30, 30),
    "Woorden met een comparatief complement" -> (31, 48),
    "Nevenschikkingen" -> (49, 70),
    "Woordgroepen" -> (71, 74),
    "Voorzetselgroepen" -> (75, 83),
    "Adjectiefgroepen" -> (84, 92),
    "Werkwoorden" -> (93, 99),
    "Scheidbare werkwoorden" -> (100, 105),
    "Werkwoordstijden" -> (106, 109),
    "Volgorde van werkwoordstijden" -> (110, 113),
    "Inbedding" -> (114, 122),
    "Topicalisatie en Extractie" -> (123, 127),
    "Parser succes" -> (128, 128),
    "Parser succes: geparste delen" -> (129, 133),
    "Parser success: overgeslagen woorden" -> (134, 138),
    "Onbekende woorden" -> (139, 144),
  )


  lazy val xpaths = io.Source.fromFile(dir + "xpaths.txt").getLines.map(x => x.split("\\t")).map(
    a => a(2) -> URLDecoder.decode(a(0), "utf-8")
  ).toMap

  lazy val indextoLabel = io.Source.fromFile(dir + "indexes.txt").getLines.map(x => x.split("\\t")).map(
    a => "i" + a(1) -> a(0)
  ).toMap

  def vindRubriek(id: String)  = {
    val i = id.replaceAll("i","").toInt
    rubrieken.find(r => i >= r._2._1  && i <= r._2._2).map(_._1).getOrElse("geen_rubriek")
  }
   lazy val checkboxes = io.Source.fromFile(dir + "name_i.txt").getLines.grouped(4).map(g => {
      val z = "<tr>" +  g.drop(1).map(x => x.replaceAll("(<input[^<>]*)>", "$1/>") + "</td>").mkString("\n") + "</tr>"
      val z1 = XML.loadString(z)
      val description = (z1 \\ "label").text
      val id = (z1 \\ "input").headOption.map(i => (i \ "@id").text).getOrElse("_")
      val label = indextoLabel.get(id).getOrElse("NO_LABEL")
      val xpath = indextoLabel.get(id).flatMap(x => xpaths.get(x)).getOrElse("NO_XPATH_FOUND")
      val rubriek = vindRubriek(id)
      id -> (label, rubriek, description,xpath)
   })

  val asHTML = <table>{checkboxes.map({case (id, (label, rubriek, description,xpath)) =>
    val xpe = URLEncoder.encode(xpath, "utf-8")
    val link = s"https://paqu.let.rug.nl:8068/xpath?db=lassysmall&xpath=$xpe&mt=std"
   <tr><td>{id}</td><td>{label}</td><td>{rubriek}</td><td>{description}</td><td><pre>{xpath}</pre></td><td><a href={link}>PaQu</a></td></tr>
  })}</table>

  def main(args: Array[String])  = {
    checkboxes.foreach(println)

    val table = new PrintWriter(dir + "asTable.html")
    table.println(asHTML.toString().replaceAll("\\s+", " "))
    table.close()
    // xpaths.foreach(println)
  }
}
