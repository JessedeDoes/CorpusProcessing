package zeeuws

import database.{Configuration, Database, DatabaseUtilities}
import database.DatabaseUtilities.Select
import utils.HTML
import utils.PostProcessXML.updateElement5

import scala.xml._
case class Lemma(r: DatabaseUtilities.Diamond) {
  def g(x: String) = r.getString(x)

  val volgnr: String = g("volgnr")
  val importnr: String = g("import")
  val orig_trefwoord: String = g("orig_trefwoord")
  val vernederl_trefwoord: String = g("vernederl_trefwoord")
  val an_trefwoord: String = g("an_trefwoord")
  val plaats: String = g("plaats")
  val orig_tekst: String = g("orig_tekst")
  val patched_tekst: String = orig_tekst
    .replaceAll("</i>(\\s+-)", "$1</i>")
    .replaceAll("(\\s+)</i>(\\s*-)", "$1$2</i>") // haal streepje binnen <i>
    .replaceAll("(-\\s*)<i>", "<i>$1")
    .replaceAll("</i>(\\s*)<i>", "$1")
    .replaceAll("</i>(\\s*)(\\(?of|A|B)(\\s*)<i>", "$1⦓span style='font-style: normal'⦔$2⦓/span⦔$3") // deze werkt niet

  // ⦓ ⦔
  lazy val parsed_html: Elem = <article_html>{(HTML.parse(patched_tekst) \\ "body").flatMap(_.child)}</article_html> // .toString()
  lazy val patched_html_node: Elem = XML.loadString(updateElement5(parsed_html, _.label == "i", i => {
    val stukjes = i.text.split(";\\s+")
    stukjes.indices.flatMap(k =>  { if (k < stukjes.length-1) Seq(<i>{stukjes(k)}</i>, scala.xml.Text("; "))  else Seq(<i>{stukjes(k)}</i>)  })
  }).asInstanceOf[scala.xml.Elem].toString()
    .replaceAll("⦔", ">")
    .replaceAll("⦓", "<")
    .replaceAll("\n", "<br/>")
    .replaceAll("\\s+", " ")
  )
  lazy val patched_html: String = patched_html_node.toString()
  if ((patched_tekst.contains("font-style: normal"))) println("SPANS:" + patched_tekst + "\nMAAR: " + patched_html_node \\ "span")
}

object VoegXHTMLToe {
  def main(args: Array[String]): Unit = {

    val c = new Configuration(name = "zeelandia", server = "svowdb20.ivdnt.loc", user = "postgres", password = "inl", database = "wnd")
    lazy val db = new Database(c)
    val q: Select[Lemma] = Select(r => Lemma(r), "lemma where boek_titel ~ 'Lievevrouw-Coopman'")
    val load = true
    val lievevrouw: Seq[Lemma] = db.slurp(q)

    if (load) {
      db.runStatement("drop table if exists lievevrouw.articles_x")

      db.runStatement("create table lievevrouw.articles_x (volgnr text, trefwoord text, xhtml xml)")
      val b = db.QueryBatch[Lemma]("insert into lievevrouw.articles_x (volgnr, trefwoord, xhtml) VALUES(:volgnr, :trefwoord, cast(:xhtml as xml))",
        l => Seq(db.Binding("volgnr", l.volgnr), db.Binding("trefwoord", l.orig_trefwoord), db.Binding("xhtml", l.patched_html)))
      b.insert(lievevrouw)

      db.runStatement("set schema 'lievevrouw'")
      db.runStatement("drop table if exists lievevrouw.lievevrouw_italics cascade")
      db.runStatement(
        """create table lievevrouw.lievevrouw_italics as
          |SELECT articles_x.volgnr,
          |   articles_x.trefwoord,
          |   articles_x.xhtml,
          |   unnest(xpath('//i'::text, lievevrouw.articles_x.xhtml)) AS cursief
          |      FROM articles_x;""".stripMargin)

      db.runStatement(""" create view cursiefjes as select cast(cursief as text) as cursief, sum(1) as aantal from lievevrouw_italics group by cast(cursief as text) order by aantal desc """)
      // en dan select volgnr, unnest(xpath('//i', xhtml)) from articles_x;
      db.runStatement("alter table lievevrouw_italics add column is_uitdrukking boolean default true")
      db.runStatement("alter table lievevrouw_italics add column uitdrukking text default null")
      db.runStatement("update lievevrouw_italics set is_uitdrukking=false where cast(cursief as text) in (select cursief from cursiefjes where aantal > 10)")
      db.runStatement(
        """update lievevrouw_italics set uitdrukking=trim(regexp_replace(array_to_string((cast(xpath('.//text()', cursief) as text[])),''), '-',
          |'[' || regexp_replace(trefwoord, ',.*', '') || ']', 'g'))""".stripMargin)
      db.runStatement("update lievevrouw_italics set is_uitdrukking=false where not (uitdrukking ~ ' ')")
      db.runStatement("alter table  lievevrouw_italics  add column id serial primary key")
    }
  }
}
