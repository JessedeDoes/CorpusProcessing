package zeeuws
import database.DatabaseUtilities.Select
import database._
import scala.xml._
import scala.util.{Try,Success,Failure}
import scala.util.matching.Regex._


case class Article(r: DatabaseUtilities.Diamond) {
  def g(x: String) = r.getString(x)
  val volgnr: String = g("volgnr")
  val importnr: String = g("import")
  val orig_trefwoord: String = g("orig_trefwoord")
  val vernederl_trefwoord: String = g("vernederl_trefwoord")
  val an_trefwoord: String = g("an_trefwoord")
  val plaats: String = g("plaats")
  val orig_tekst: String = g("orig_tekst")

  lazy val parsed_html = <article_html>{(HTML.parse(orig_tekst) \\ "body")}</article_html>

  val patched_text: String = orig_tekst.replaceAll("<(BR|br)[^<>]*>", "<br/>").replaceAll("&#?[A-Za-z0-9]+;", "")

  lazy val preparsed_0: String = patched_text
    .replaceAll("(<br/>\\s*)([0-9]+)\\.", "\n<sense n='$2'/>")
    .replaceAll("</b>([a-z\\. ]){0,10}1\\.", "</b><pos>$1</pos>\n<sense n='1'/>")
    .replaceAll("^([a-z\\.' ]{0,10})<b>(.*?)</b>", "<lid>$1</lid>\n<lemma>$2</lemma>")

  lazy val senses_marked: String =   {
     val chunks = preparsed_0.replaceAll("<sense", "意味<sense").replaceAll("<sense([^<>]*)/>", "<sense$1>").split("意味")
     chunks(0) + chunks.tail.map(c => c + "</sense>").mkString("\n")
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
  val entity_map = Map(
    "frac12" -> "1/2",
    "hellip" -> "...",
    "fnof" -> ""
  )
  val c = new Configuration(name="zeelandia", server="svowdb20.ivdnt.loc", user="postgres", password="inl", database = "wnd")
  lazy val db = new Database(c)
  val q: Select[Article] = Select(r => Article(r), "lemmata_wzd")
  lazy val zeeuws: Seq[Article] = db.slurp(q)
  lazy val xml: Elem = <dictionary>{zeeuws.filter(_.wellformed).map(_.article_xml.get)}</dictionary>
  lazy val html = <dictionary>{zeeuws.map(_.parsed_html)}</dictionary>
  val pretty = new scala.xml.PrettyPrinter(300, 4)
  def main(args: Array[String]): Unit = {
    val all_entities = zeeuws.flatMap(_.entities).toSet
    println(all_entities)
    val misbaksels = zeeuws.filter(z => !z.wellformed)

    println(s"!  ${misbaksels.size} Failures  !")
    misbaksels.take(3).foreach(z => println("\n" + z.article_xml  + "\n" + z.senses_marked))
    //XML.save("/tmp/wzd.xml", xml)
    new java.io.PrintWriter("/tmp/wzd.xml").println(pretty.format(xml))
    new java.io.PrintWriter("/tmp/wzd_html.xml").println(pretty.format(html))
  }
}
