package corpusprocessing.gekaapte_brieven

import corpusprocessing.gekaapte_brieven.Article.meta

case class Metadata(fields: Map[String, String], participants: List[Participant] = List(), isGroupMetadata: Boolean = false, groupMemberIds: List[String]  = List()) {
  def apply(f: String): String = if (fields.contains(f) && fields(f) != null) fields(f) else "unknown"

  def contains(n: String)  = fields.contains(n)  && fields(n) != null && fields(n).nonEmpty && fields(n) != "unknown"
  def ->(f: String) = this (f)

  lazy val allMeta = fields.filter({ case (n, v) => v != null && v.length < 100 && v.nonEmpty && !Set("t", "f").contains((v)) }).toList.sorted.map({ case (n, v) => meta(n, v) })


  // beetje gerotzooi vanwege multiple values bij groepjes
  def ymd(x: String): (String, String, String) = {
    val datering = x.split("-").map(x => if (x.matches("^0+$")) "unknown" else x)
    if (datering.size >= 3)
      if (datering(0).size == 4) (datering(0), datering(1), datering(2)) else (datering(2), datering(1), datering(0))
    else
      ("unknown", "unknown", "unknown")
  }

  lazy val (year, month, day) = {
    def z(x: Array[String]) = x.toSet.filter(_ != "unknown").toList.sorted.mkString(";")
    val many: Array[(String, String, String)] = (this -> "datering_text").split("\\s*;\\s*").map(ymd)
    (z(many.map(_._1)), z(many.map(_._2)), z(many.map(_._3)))
  }

  lazy val senders = participants.filter(_.typ == "afzender")
  lazy val recipients = participants.filter(_.typ == "ontvanger")

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
        {if (!isGroupMetadata) meta("pid", s"letter.${this -> "brief_id"}")}
        {meta("sourceId", this -> "archiefnummer_xln")}
        {meta("sourceURL", this -> "originele_vindplaats_xln")}
        {meta("level2.id", this -> "groepID_INT")}
        {meta("witnessYear_from", year)}
        {meta("witnessYear_to", year)}
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

    val nonPersonMetadata: Set[(String, String)] = fields
        .filter(x => !x.matches("^(afz|ontv).*"))
        .map(n => n -> metadatas.map(m => m(n))
        .filter(_.nonEmpty).toSet.mkString(";"))

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
    baseMeta.copy(fields = nonPersonMetadata.toMap, participants = afzenders ++ ontvangers, isGroupMetadata = true, groupMemberIds =  members)
  }
}