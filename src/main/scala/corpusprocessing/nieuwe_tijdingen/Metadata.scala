package corpusprocessing.nieuwe_tijdingen
import scala.xml._

object Metadata {
  def convertTeiMetadataToTsv(doc: Elem): String = {
    // Inlezen van de string als XML-document


    // Verzamelen van alle <bibl>-elementen binnen <listBibl>
    val bibls = doc \\ "listBibl" \\ "bibl"

    // Definieer de vaste volgorde en namen van de velden (kolommen)
    val fieldOrder = List(
      "titleLevel1",
      "titleLevel2",
      "corpusProvenance",
      "witnessYearLevel1_from",
      "witnessMonthLevel1_from",
      "witnessDayLevel1_from",
      "witnessYearLevel1_to",
      "witnessMonthLevel1_to",
      "witnessDayLevel1_to"
    )

    // Bouw de header voor de TSV (optioneel)
    val header = fieldOrder.mkString("\t")

    // Construeer per <bibl> een regel met de gevraagde velden
    val rows = bibls.map { bibl =>
      // Map van type -> inhoud, bv. "titleLevel1" -> "ZN kranten 17: ..."
      val typeToValue = (bibl \\ "interpGrp").map { interpGrp =>
        val tpe = (interpGrp \ "@type").text.trim
        val content = (interpGrp \\ "interp").text.trim
        tpe -> content
      }.toMap

      // Per veldnaam (in fieldOrder) pak je de bijbehorende tekst als die bestaat, anders leeg
      fieldOrder.map(fieldName => typeToValue.getOrElse(fieldName, "")).mkString("\t")
    }

    // Header + data onder elkaar in TSV-formaat
    (header +: rows).mkString("\n")
  }
}
