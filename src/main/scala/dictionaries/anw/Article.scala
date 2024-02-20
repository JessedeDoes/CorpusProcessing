package dictionaries.anw

import scala.collection.immutable
import scala.xml.{Node, XML}

case class Article(doc: String) {
  val molexIdField = "SpellingDbId"

  lazy val article = XML.loadString(doc)
  lazy val article_pid =  (article \ "@pid").text

  lazy val betekenissen =  (article \\ "Kernbetekenis") ++ (article \\ "Subbetekenis")

  lazy val defs = betekenissen.flatMap(x => x \ "betekenisInfo" \ "definitieBody" \ "Definitie")
    .map(_.text.replaceAll("\\s+", " ").trim)
    .mkString("; ")

  lazy val lemma = (article \ "Lemma" \\ "Lemmavorm").map(_.text).mkString("|")
  lazy val woordsoort = (article \ "Woordsoort" \ "Type").text

  lazy val anw_molex_in_betekenissen = betekenissen.map(b =>
    (
      article_pid,
      lemma,
      woordsoort,
      Some((b \ "@pid").text),
      (b \ "betekenisInfo" \ "SpellingDbId").text,
      (b \ "betekenisInfo" \ "definitieBody" \ "Definitie").text.replaceAll("\\s+", " ").trim
    )
  ).toList

  lazy val anw_molex_restje = (article \\ molexIdField)
    .map(_.text).toSet
    .diff(anw_molex_in_betekenissen.map(_._5).toSet)
    .map(p => (
      article_pid,
      lemma,
      woordsoort,
      None,
      p,
      "no_def_found_for_this_pid"
    ))

  lazy val anw_molex =  anw_molex_in_betekenissen ++ anw_molex_restje
}


/*
		<Kernbetekenis pid="578256">
			<betekenisInfo>
				<Betekenisnummer>1.0</Betekenisnummer>
				<Lemma>
					<Lemmavorm>fietsblogger</Lemmavorm>
					<Lemmatype>woord</Lemmatype>
				</Lemma>
				<SpellingDbId>903243</SpellingDbId>
 */
