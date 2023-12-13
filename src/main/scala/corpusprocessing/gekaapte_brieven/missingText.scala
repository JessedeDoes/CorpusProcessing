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
    articlesUnfiltered.filter(_.textMissing).sortBy(a => a -> "archiefnummer_xln").foreach(a => {
      val archiefnr = a -> "archiefnummer_xln"
      val isExcel =  a -> "excel"
      val notes = (a.xml \\ "note").filter(n => (n \ "@resp").nonEmpty).map(_.text.trim.replaceAll("\\s+", " ")).mkString("; ")
      val excelIsMentioned = notes.contains("nl-hana")
      val excel = if (excelIsMentioned) notes.replaceAll(".*(nl[_-]hana\\S+)", "$1") else {
        archiefnr
      }
      val pogingTotXml = baseName(excel)
      val isEr = baseNames.contains(pogingTotXml) /// new File(pogingTotXml).exists()
      val actualFiles: io.Serializable = baseNames.get(pogingTotXml).getOrElse(Array()).mkString("|")
      // archiefnummer brief_id genre notes excel_mentioned base_name found matching
      pw.println(s"$archiefnr\t${a.id}\t$isExcel\t${a.metadata.genre}\t${notes}\t$excel\t$pogingTotXml\t$excelIsMentioned\t$isEr\t$actualFiles\t")
      // a.metadata.report()
    })
    pw.close()
  }
}
