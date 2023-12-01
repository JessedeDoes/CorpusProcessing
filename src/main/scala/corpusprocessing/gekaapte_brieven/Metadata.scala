package corpusprocessing.gekaapte_brieven

import corpusprocessing.gekaapte_brieven.Metadata.meta

case class Metadata(fields: Map[String, String], participants: List[Participant] = List(), isGroupMetadata: Boolean = false, groupMemberIds: List[String]  = List(), groupMetadata:Option[Metadata] = None) {
  def apply(f: String): String = if (fields.contains(f) && fields(f) != null) fields(f) else "unknown"

  def contains(n: String)  = fields.contains(n)  && fields(n) != null && fields(n).nonEmpty && fields(n) != "unknown"
  def ->(f: String) = this (f)

  lazy val allMeta = fields.filter({ case (n, v) => v != null && v.length < 100 && v.nonEmpty && !Set("t", "f").contains((v)) }).toList.sorted.map({ case (n, v) => meta(n, v) })

  lazy val broertjes: Seq[String] = groupMetadata.map(_.groupMemberIds).getOrElse(List())

  // beetje gerotzooi vanwege multiple values bij groepjes
  def ymd(x: String): (String, String, String) = {
    val datering = x.split("-").map(x => if (x.matches("^0+$")) "unknown" else x)
    if (datering.size >= 3)
       (datering(0), datering(1), datering(2))
    else
      ("unknown", "unknown", "unknown")
  }

  lazy val (years, months, days) = {
    def z(x: Array[String]) = x.toSet.filter(_ != "unknown").toList.sorted

    val many: Array[(String, String, String)] = (this -> "datering_text").split("\\s*;\\s*").map(ymd)
    (z(many.map(_._1)), z(many.map(_._2)), z(many.map(_._3)))
  }

  lazy val yearsPlus: Iterable[String] = if (years.exists(_ != "unknown")) years.filter(_ != "unknown") else groupMetadata.map(_.years).getOrElse(List()).sorted


  lazy val minYear = yearsPlus.filter(_ != "unknown").headOption.getOrElse("unknown")
  lazy val maxYear = yearsPlus.filter(_ != "unknown").lastOption.getOrElse("unknown")

  lazy val this_datering = if (minYear == maxYear) minYear else s"$minYear-$maxYear"
  lazy val datering: String = if (this_datering == "unknown") groupMetadata.map(_.datering).getOrElse("unknown") else this_datering
  
  //lazy val datering = if (this.contains("witnessYear_from")) this("witnessYear_from") else groupMetadata.map(_.datering).getOrElse("ongedateerd")


  lazy val genre = if (this.contains("tekstsoort_INT")) this("tekstsoort_INT") else groupMetadata.map(_("tekstsoort_INT")).getOrElse("onbekende tekstsoort")

  def report() = if (!isGroupMetadata) {
    println(s"datering ($isGroupMetadata): $yearsPlus -> $datering!, genre= $genre, page level genre= ${this -> "tekstsoort_INT"}, broertjes=$broertjes")
  }

  lazy val (year, month, day) = {
    def z(x: Array[String]) = x.toSet.filter(_ != "unknown").toList.sorted.mkString(";")
    val many: Array[(String, String, String)] = (this -> "datering_text").split("\\s*;\\s*").map(ymd)
    (z(many.map(_._1)), z(many.map(_._2)), z(many.map(_._3)))
  }

  lazy val senders: Seq[Participant] = participants.filter(_.typ == "afzender")
  lazy val recipients: Seq[Participant] = participants.filter(_.typ == "ontvanger")

  lazy val TEI = <sourceDesc xml:id={if (isGroupMetadata) "metadata.level2" else "metadata.level0"}>
    <listBibl type={if (isGroupMetadata) "intCorpusMetadata.level2" else "intCorpusMetadata.level0"}>
      <bibl>
        {if (isGroupMetadata) <note>Grouped metadata, members: {groupMemberIds.toList.sorted.mkString(", ")}</note>}
        <note>
          {this -> "bronvermelding_xln"}
        </note>
        <note resp="editor">
          {this -> "adressering_xl"}
        </note>
        {if (!isGroupMetadata) meta("pid", s"letter_${this -> "brief_id"}")}
        {meta("sourceID", this -> "archiefnummer_xln")}
        {meta("sourceURL", this -> "originele_vindplaats_xln")}
        {meta("level2.id", this -> "groepID_INT")}
        {meta("year", datering)}
        {meta("witnessYear_from", minYear)}
        {meta("witnessYear_to", maxYear)}
        {meta("witnessMonth_from", month)}
        {meta("witnessMonth_to", month)}
        {meta("witnessDay_from", day)}
        {meta("witnessDay_to", day)}
        {meta("language", this -> "taal_INT")}
        {meta("genre", this -> "tekstsoort_INT")}
      </bibl>
    </listBibl>{if (senders.nonEmpty) <listPerson type="afzenders">
      <desc>Lists the senders or creators</desc>{senders.map(_.xml)}
    </listPerson>}{if (recipients.nonEmpty) <listPerson type="ontvangers">
      <desc>Lists the recipients</desc>{recipients.map(_.xml)}
    </listPerson>}
  </sourceDesc>
}

object Metadata {

  import corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.grouping.groupWithFirst
  def splitIntoSequences(metadatas: List[Metadata]): List[List[Metadata]] = {
    val ids: Seq[(Int, Int)] = metadatas.map(x => x("brief_id")).map(_.toInt).sorted.zipWithIndex
    def gat(ki: (Int,Int))  = ki match { case (k,i) => i==0 || ids(i-1)._1 != k-1 }
    val groepjes = groupWithFirst[(Int,Int)](ids, gat)
    val idGroups = groepjes.map(x => x.map(_._1.toString))
    if (idGroups.size > 1) {
      Console.err.println(s"Whahoooo $idGroups")
    }
    idGroups.map(g => metadatas.filter(m => g.contains(m("brief_id")))).toList
  }

  def groupMetadata(metadatas: List[Metadata]): Metadata = {
    val baseMeta = metadatas.head
    val fields = baseMeta.fields.keySet

    val nonPersonMetadata: Map[String, String] = fields
        .filter(x => !x.matches("^(afz|ontv).*"))
        .map(n => n -> metadatas.map(m => m(n))
        .filter(_.nonEmpty).toSet.mkString(";")).toMap

    // println(s"Group: ${metadatas.size}, ${nonPersonMetadata.get("tekstsoort_INT")}")

    val afzenders = metadatas.filter(a => a.contains("afz_id") && a("afz_id").nonEmpty).groupBy(a => a("afz_id")).mapValues(arts => {
      val a = arts.head
      val f = a.fields.filter(_._1.startsWith("afz")).filter({case (n,v) => v != null && n.nonEmpty}).map({ case (n, v) =>
        val value = arts.map(x => x(n)).filter(_.nonEmpty).toSet.mkString("; ")
        n -> value
      })
      Participant("afzender", f)
    }).values.toList

    val ontvangers = metadatas.filter(a => a.contains("ontv_id") && a("ontv_id").nonEmpty).groupBy(a => a("ontv_id")).mapValues(arts => {
      val a = arts.head
      val f = a.fields.filter(_._1.startsWith("ontv")).filter({case (n,v) => v != null && n.nonEmpty}).map({ case (n, v) =>
        val value = arts.map(x => x(n)).filter(_.nonEmpty).toSet.mkString("; ")
        n -> value
      })
      Participant("ontvanger", f)
    }).values.toList

    val members = metadatas.map(x => x -> "brief_id")
    baseMeta.copy(fields = nonPersonMetadata, participants = afzenders ++ ontvangers, isGroupMetadata = true, groupMemberIds =  members)
  }

  def meta(n: String, v: String)  = {
    val values_split = v.split("\\s*;\\s*").toSet.toList.sorted
    <interpGrp type={n}>{values_split.map(v => <interp>{v}</interp>)}</interpGrp>
  }
}