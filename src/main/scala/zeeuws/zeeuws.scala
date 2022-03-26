package zeeuws
import database.DatabaseUtilities.Select
import database._
import utils.PostProcessXML

import scala.xml._
import scala.util.{Failure, Success, Try}
import scala.util.matching.Regex._
import utils.PostProcessXML.updateElement

case class Article(r: DatabaseUtilities.Diamond) {
  def g(x: String) = r.getString(x)
  val volgnr: String = g("volgnr")
  val importnr: String = g("import")
  val orig_trefwoord: String = g("orig_trefwoord")
  val vernederl_trefwoord: String = g("vernederl_trefwoord")
  val an_trefwoord: String = g("an_trefwoord")
  val plaats: String = g("plaats")
  val orig_tekst: String = g("orig_tekst")
  val korte_betekenissen: Seq[String] = an_trefwoord.split("\\s*;\\s*").toList

  lazy val parsed_html = <article_html>{(HTML.parse(orig_tekst) \\ "body").flatMap(_.child)}</article_html>

  val patched_text: String = orig_tekst.replaceAll("<(BR|br)[^<>]*>", "<br/>").replaceAll("&#?[A-Za-z0-9]+;", "")
  val patched_html: String = parsed_html.child.map(_.toString()).mkString("").replaceAll("<(br|BR)[^<>]*>", "<br/>")

  lazy val senses_marked: String =   {

    lazy val preparsed_0: String = patched_html
      .replaceAll("(<br/>\\s*)([0-9]+)\\.", "\n<sense n='$2'/>")
      .replaceAll("</b>([a-z\\. ]){0,10}1\\.", "</b><pos>$1</pos>\n<sense n='1'/>")
      .replaceAll("^([a-z\\.' ]{0,10})<b>(.*?)</b>", "<lid>$1</lid>\n<lemma>$2</lemma>")


    // voeg een eerste sense toe als er een 1. in her artikel voor een andere wel gevonden <sense staat

    val preparsed_1 : String = if (preparsed_0.contains("<sense") && (! preparsed_0.contains("<sense n='1'"))
      && preparsed_0.contains("1.") && "1\\.".r.findFirstMatchIn(preparsed_0).get.start < "<sense".r.findFirstMatchIn(preparsed_0).get.start)
      preparsed_0.replace("1.", "<sense bijgeflieberd='true' n='1'/>")
    else preparsed_0 // alleen als de 1. voor de eerste <sense staat .....

    // voeg een eerste sense toe als er wel een sense is, maar geen 1

    val preparsed_2: String = if (preparsed_1.contains("<sense") && (! preparsed_1.matches("(?s).*<sense[^<>]*n='1'.*"))) {
      preparsed_1.replaceAll("</lemma>", "</lemma>\n<sense type='bijgeflabberd' n='1'/>")
    }  else preparsed_1

    var preparsed_3 : String = if (preparsed_2.contains("<sense")) preparsed_2 else preparsed_2.replaceAll("</lemma>", "</lemma>\n<sense type='bijgeflobberd' n='1'/>")

    val chunks = preparsed_3.replaceAll("<sense", "意味<sense").replaceAll("<sense([^<>]*)/>", "<sense$1>").split("意味")
    chunks(0) + chunks.tail.map(c => c + "</sense>").mkString("\n")
  }

  lazy val met_korte_betekenissen: Try[Elem] = {
    if (!wellformed || korte_betekenissen.size != (article_xml.get \\ "sense").size) article_xml else
    {
       val s1 = PostProcessXML.updateElement(article_xml.get, _.label == "sense", s => {
         val index = (s \ "@n").text.toInt -1
         Console.err.println(s"$index $korte_betekenissen  --> ${korte_betekenissen.size}")
        val kb = korte_betekenissen(index)
        s.copy(child = <def type="an_trefwoord">{kb}</def> +: s.child)
      })
      Success(s1)
    }
  }

  lazy val kopje: Elem = <kopje>
    <orig_trefwoord>{orig_trefwoord}</orig_trefwoord>
    <importnr>{importnr}</importnr>
    <vernederl_trefwoord>{vernederl_trefwoord}</vernederl_trefwoord>
    <an_trefwoord>{an_trefwoord}</an_trefwoord>
  </kopje>

  lazy val article_xml: Try[Elem] = Try(XML.loadString(s"<article n='$volgnr'>" + kopje.toString() + senses_marked + "</article>")) // .getOrElse(parsed_html)
  val entities: Set[String] = "&#?[A-Za-z0-9]+;".r.findAllIn(orig_tekst).map(_.toString).toSet
  lazy val wellformed: Boolean = article_xml match {case Success(x) => true; case _ => false}
}

object zeeuws {
 // Set(&rdquo;, &frac12;, &hellip;, &fnof;, &frac14;, &sup1;, &deg;, &mdash;, &sup3;, &bdquo;, &#8531;, &sup2;, &plusmn;, &emsp;, &lsquo;, &ldquo;, &nbsp;, &#8539;, &ndash;, &rsquo;)

  val c = new Configuration(name="zeelandia", server="svowdb20.ivdnt.loc", user="postgres", password="inl", database = "wnd")
  lazy val db = new Database(c)
  val q: Select[Article] = Select(r => Article(r), "lemmata_wzd")
  lazy val zeeuws: Seq[Article] = db.slurp(q)
  lazy val xml: Elem = <dictionary>{zeeuws.filter(_.wellformed).map(_.met_korte_betekenissen.get)}</dictionary>
  lazy val html = <dictionary>{zeeuws.map(_.parsed_html)}</dictionary>
  val pretty = new scala.xml.PrettyPrinter(300, 4)

  def main(args: Array[String]): Unit = {
    val all_entities = zeeuws.flatMap(_.entities).toSet
    println(all_entities)
    val misbaksels = zeeuws.filter(z => !z.wellformed)

    println(s"!  ${misbaksels.size} Failures  !")
    misbaksels.take(3).foreach(z => println("\n" + z.article_xml  + "\n" + z.senses_marked))
    //XML.save("/tmp/wzd.xml", xml)
    val p1 = new java.io.PrintWriter("/tmp/wzd.xml")
    val p2 = new java.io.PrintWriter("/tmp/wzd_html.pretty.xml")
    val p3 = new java.io.PrintWriter("/tmp/wzd_html.xml")
    p1.println(pretty.format(xml))
    p2.println(pretty.format(html))
    p3.println(html)
    List(p1,p2,p3).foreach(_.close())
  }
}
