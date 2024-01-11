package corpusprocessing.gekaapte_brieven

import corpusprocessing.gekaapte_brieven.Settings.extraXMLFromExcel
import corpusprocessing.gekaapte_brieven.exportCorpus.articlesUnfiltered

import java.io
import java.io.{File, PrintWriter}

object missingText {

  import exportCorpus.articlesWithGroupMetadata

  def baseName(f: String) = f.replaceAll(".xml$", "").replaceAll("(nl-hana_hca[0-9](.*?)[0-9]{4}).*", "$1")

  def main(args: Array[String]) = {
    exportCorpus.init()

    val pw = new PrintWriter("/tmp/noText.txt")
    val extraXMLs = new File(extraXMLFromExcel).listFiles().filter(_.getName.endsWith(".xml"))
    val baseNames: Map[String, Array[String]] = extraXMLs
      .map(_.getName.replaceAll(".xml$", ""))
      .map(x => (baseName(x), x))
      .groupBy(_._1)
      .mapValues(_.map(_._2)) // .map(_.replaceAll("[-,][0-9]+", "").replaceAll("[_c]$", ""))

    // baseNames.foreach(println)
    val infos = articlesUnfiltered.filter(_.textMissing).sortBy(a => a -> "archiefnummer_xln").map(a => {
      val archiefnr = a -> "archiefnummer_xln"
      val isExcel =  a -> "excel"
      val notes = (a.xml \\ "note").map(_.text.trim.replaceAll("\\s+", " ")).mkString("; ")
      val txt = a.mainTextDiv.text.replaceAll("\\s+", " ").trim
      val excelIsMentioned = notes.contains("nl-hana")
      val excel = if (excelIsMentioned) notes.replaceAll(".*(nl[_-]hana\\S+)", "$1") else {
        archiefnr
      }
      val pogingTotXml = baseName(excel)
      val isEr = baseNames.contains(pogingTotXml) /// new File(pogingTotXml).exists()
      val actualFiles: io.Serializable = baseNames.get(pogingTotXml).getOrElse(Array()).mkString("|")
      // archiefnummer brief_id genre notes excel_mentioned base_name found matching

      // archiefnummer id genre notes excel xml excel_mentioned excel_found matches is_excel text
      val fields = List("archiefnummer" -> archiefnr, "id" -> a.id, "genre" -> a.metadata.genre, "taal" -> (a.metadata -> "taal_INT"), "notes" -> notes, "text" -> txt, "comment" -> "")

      // pw.println(s"$archiefnr\t${a.id}\t${a.metadata.genre}\t${notes}\t$excel\t$pogingTotXml\t$excelIsMentioned\t$isEr\t$actualFiles\t$isExcel\t$txt")
      // a.metadata.report()
      fields
    })

    pw.println(infos.head.map(_._1).mkString("\t"))
    infos.foreach(fields => pw.println(fields.map(_._2).mkString("\t")))
    pw.close()
  }
}
