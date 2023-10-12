package zeeuws.xml2db

import scala.xml._
object Classes {

  // Database classes

  case class Lemma(sense_id: String, lemma_wzd: String, lemma: String, definition: String)

  case class Keyword(lemma_id: String, keyword: String, keyword_id: String) {
    lazy val id = keyword_id
  }

  case class Response(keyword_id: String, keyword: String, place: String, isRegion: Boolean)

  // XML-based classes
  case class Entry(e: Node) {
    val entryLevelFields = List("volgnr", "import", "orig_trefwoord", "vernederl_trefwoord", "an_trefwoord").map(f => f -> (e \ f).text).toMap
    val headword = entryLevelFields("orig_trefwoord")
    val lemma = entryLevelFields("an_trefwoord")
    val senses = (e \\ "sense").map(s => Sense(s, this))
  }

  case class Sense(s: Node, e: Entry, nr: Option[String] = None, defi: Option[String] = None) {
    val volgnr = nr.getOrElse((s \ "@nr").text)
    val id = e.entryLevelFields("volgnr") + "." + volgnr
    val definition = defi.getOrElse((s \ "definition").text)
    val variants = (s \ "var").map(v => variant(v, this))
    val usages = (s \ "usg").map(u => usage(u, this)) ++ variants.flatMap(v => v.usages)
    val keywords = usages.map(_.keyword).toSet.zipWithIndex.map({ case (w, i) => Keyword(id, w, s"$id.$i") })
    val kwMap = keywords.map(k => k.keyword -> k).toMap
    lazy val lemma = Lemma(id, e.headword, e.lemma, definition)
    lazy val attestations = usages.flatMap(_.attestations)
    lazy val splitMe = if (definition.contains(";")) definition.split("\\s*;\\s*").zipWithIndex.map({ case (d, i) => Sense(s, e, Some(volgnr + "_" + i), Some(d + " YEP! ")) }).toSeq else Seq(this)
  }

  case class variant(v: Node, s: Sense) {
    val keyword = (v \ "v").text

    def usages = (v \ "usg").map(u => usage(u, s, Some(this)))
  }

  case class Attestation(s: Sense, u: usage, p: place) {
    val e = s.e
    val sense_id = s.id
    val keywordText = u.keyword
    lazy val keyword = s.kwMap(keywordText)

    override def toString() = s"$sense_id\t$keywordText\t${p.name}"

    lazy val response = Response(keyword.id, keywordText, p.name, p.isRegion) //
  }

  case class usage(u: Node, s: Sense, variant: Option[variant] = None) {
    val keyword = variant.map(_.keyword).getOrElse(s.e.entryLevelFields("orig_trefwoord"))
    val places = (u.child.filter(x => Set("region", "placeName").contains(x.label) && !((x \ "@use").text == "false"))).map(place)
    val attestations = places.map(p => Attestation(s, this, p))
  }


  case class place(p: Node) {
    val name = p.text
    val isRegion = p.label == "region"
  }
}
