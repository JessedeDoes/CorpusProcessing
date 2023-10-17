package zeeuws.xml2db

import scala.collection.immutable
import scala.xml._
object Classes {

  // Database classes

  case class Lemma(sense_id: String, lemma_wzd: String, lemma: String, definition: String) {
    def lemma_voor_db = s"($lemma_wzd), AN: $lemma"
  }

  case class Keyword(lemma_id: String, keyword: String, keyword_an: String, keyword_id: String, isVariant: Boolean = false) {
    lazy val id: String = keyword_id
  }

  case class Response(keyword_id: String, keyword: String, place: String, isRegion: Boolean)

  // XML-based classes
  case class Entry(e: Node) {
    val entryLevelFields: Map[String, String] = List("volgnr", "import", "orig_trefwoord", "vernederl_trefwoord", "an_trefwoord").map(f => f -> (e \ f).text).toMap
    val headword: String = entryLevelFields("orig_trefwoord")
    val lemma: String = entryLevelFields("an_trefwoord")
    val senses: immutable.Seq[Sense] = (e \\ "sense").map(s => Sense(s, this))
  }

  case class Sense(s: Node, e: Entry, nr: Option[String] = None, defi: Option[String] = None) {
    val volgnr: String = nr.getOrElse((s \ "@nr").text)
    val id: String = e.entryLevelFields("volgnr") + "." + volgnr
    val definition: String = defi.getOrElse((s \ "definition").text)
    val variants: immutable.Seq[variant] = (s \ "var").map(v => variant(v, this))
    val usages: immutable.Seq[usage] = (s \ "usg").map(u => usage(u, this)) ++ variants.flatMap(v => v.usages)
    val keywords: Set[Keyword] = usages.map(_.keyword).toSet.zipWithIndex.map({ case (w, i) =>w.copy(keyword_id =  s"$id.$i") })
    val kwMap: Map[String, Keyword] = keywords.map(k => k.keyword_an -> k).toMap
    lazy val lemma: Lemma = Lemma(id, e.headword, e.lemma, definition)
    lazy val attestations: immutable.Seq[Attestation] = usages.flatMap(_.attestations)
    // lazy val splitMe: Seq[Sense] = if (definition.contains(";")) definition.split("\\s*;\\s*").zipWithIndex.map({ case (d, i) => Sense(s, e, Some(volgnr + "_" + i), Some(d + " YEP! ")) }).toSeq else Seq(this)
  }

  case class variant(v: Node, s: Sense) {
    val keyword: String = (v \\ "v").headOption.getOrElse(v \\ "i").headOption.map(_.text).getOrElse("no_keyword_in_variant") // zit soms in de <usg>, soms niet
    if (keyword == "no_keyword_in_variant") {
      Console.err.println(s"Geen vorm in variant ${v}")
    }
    def usages: Seq[usage] = (v \ "usg").map(u => usage(u, s, Some(this)))
  }

  case class Attestation(s: Sense, u: usage, p: place) {
    val e: Entry = s.e
    val sense_id: String = s.id
    val keywordText: String = u.keyword.keyword_an
    lazy val keyword: Keyword = s.kwMap(keywordText)

    override def toString: String = s"$sense_id\t$keywordText\t${p.name}"

    lazy val response: Response = Response(keyword.id, keywordText, p.name, p.isRegion) //
  }

  case class usage(u: Node, s: Sense, variant: Option[variant] = None) {
    val org_keyword: String = variant.map(_.keyword).getOrElse(s.e.entryLevelFields("orig_trefwoord"))
    val an_keyword: String  = if (variant.nonEmpty) org_keyword else s.e.entryLevelFields("vernederl_trefwoord")
    val hasVariant: Boolean = variant.nonEmpty
    val keyword: Keyword = Keyword(s.id,org_keyword,an_keyword,"nope", hasVariant)
    val places: Seq[place] = (u.child.filter(x => Set("region", "placeName").contains(x.label) && !((x \ "@use").text == "false"))).map(place)
    val attestations: Seq[Attestation] = places.map(p => Attestation(s, this, p))
  }


  case class place(p: Node) {
    val name: String = p.text
    val isRegion: Boolean = p.label == "region"
  }
}
