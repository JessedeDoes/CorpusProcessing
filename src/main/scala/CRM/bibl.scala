package CRM
import scala.xml._

object bibl
{

   def value(interpGrp: Node):String = (interpGrp \ "interp").map(_.text.trim).mkString("|")
   def name(interpGrp: Node): String = (interpGrp \ "@type").text.trim


   def mergeBibl(instance: Elem):Elem =
   {
      val instanceValues = (instance \ "interpGrp").map(i => name(i) -> value(i)).filter(_._2.nonEmpty)
      val realStuff = instanceValues.map(_._1).toSet
      val filledOut = (template \ "interpGrp").map(i => 
        {
          if (realStuff.contains( name(i) )) (instance \ "interpGrp").filter(i1 => name(i1) == name(i)).head
          else i
        }).filter(i => value(i).nonEmpty && !value(i).matches("\\{.*\\}")).filter(i => !name(i).contains("Level3")) // blokkeer voorlopig level 3

      template.copy(child = filledOut ++ template.child.filter(n => n.label != "interpGrp"))
   }

val template = 
  <bibl>
    <interpGrp type="sourceID">
      <interp>corpusgysseling.0001</interp>
    </interpGrp>
    <interpGrp type="pid">
      <interp></interp>
    </interpGrp>
    <interpGrp type="titleLevel1">
      <interp></interp>
    </interpGrp>
    <interpGrp type="titleLevel2">
      <interp>Corpus van Reenen-Mulder</interp>
    </interpGrp>
    <interpGrp type="titleLevel3">
      <interp>Corpus van Reenen-Mulder</interp>
    </interpGrp>
    <interpGrp type="subtitleLevel3">
      <interp>Een gegevensbank van 14de-eeuwse Middelnederlandse dialecten</interp>
    </interpGrp>
    <interpGrp type="subtitleLevel1">
      <interp/>
    </interpGrp>
    <interpGrp type="subtitleLevel2">
      <interp>Een gegevensbank van 14de-eeuwse Middelnederlandse dialecten</interp>
    </interpGrp>
    <interpGrp type="authorLevel1">
      <interp/>
    </interpGrp>
    <interpGrp type="authorLevel2">
      <interp/>
    </interpGrp>
    <interpGrp type="authorLevel3">
      <interp/>
    </interpGrp>
    <interpGrp type="editorLevel1">
      <interp>Pieter van Reenen</interp>
      <interp>Maaike Mulder</interp>
    </interpGrp>
    <interpGrp type="editorLevel2">
      <interp>Pieter van Reenen</interp>
      <interp>Maaike Mulder</interp>
    </interpGrp>
    <interpGrp type="editorLevel3">
      <interp>Pieter van Reenen</interp>
      <interp>Maaike Mulder</interp>
    </interpGrp>
    <interpGrp type="categoryLevel1">
      <interp>uitgave</interp>
    </interpGrp>
    <interpGrp type="categoryLevel2">
      <interp>uitgave</interp>
    </interpGrp>
    <interpGrp type="categoryLevel3">
      <interp>uitgave</interp>
    </interpGrp>
    <interpGrp type="editionLevel1">
      <interp>1</interp>
    </interpGrp>
    <interpGrp type="editionLevel2">
      <interp>1</interp>
    </interpGrp>
    <interpGrp type="editionLevel3">
      <interp>1</interp>
    </interpGrp>
    <interpGrp type="mediumLevel1">
      <interp>digitale uitgave</interp>
    </interpGrp>
    <interpGrp type="mediumLevel2">
      <interp>digitale uitgave</interp>
    </interpGrp>
    <interpGrp type="mediumLevel3">
      <interp>digitale uitgave</interp>
    </interpGrp>
    <interpGrp type="resourceURILevel1">
      <interp>http://www.diachronie.nl/corpora/crm14</interp>
    </interpGrp>
    <interpGrp type="resourceURILevel2">
      <interp>http://www.diachronie.nl/corpora/crm14</interp>
    </interpGrp>

    <interpGrp type="placePublicationLevel1">
      <interp></interp>
    </interpGrp>
    <interpGrp type="placePublicationLevel2">
      <interp>Amsterdam</interp>
    </interpGrp>
    <interpGrp type="placePublicationLevel3">
      <interp>Amsterdam</interp>
    </interpGrp>
    <interpGrp type="publisherLevel1">
      <interp>Pieter van Reenen</interp>
    </interpGrp>
    <interpGrp type="publisherLevel2">
      <interp>Pieter van Reenen</interp>
    </interpGrp>
    <interpGrp type="publisherLevel3">
      <interp>Pieter van Reenen</interp>
    </interpGrp>
    <interpGrp type="witnessYearLevel1_from">
      <interp></interp>
    </interpGrp>
    <interpGrp type="witnessYearLevel1_to">
      <interp></interp>
    </interpGrp>
    <interpGrp type="witnessYearLevel2_from">
      <interp>1300</interp>
    </interpGrp>
    <interpGrp type="witnessYearLevel2_to">
      <interp>1400</interp>
    </interpGrp>
    <interpGrp type="witnessYearLevel3_from">
      <interp> </interp>
    </interpGrp>
    <interpGrp type="witnessYearLevel3_to">
      <interp/>
    </interpGrp>
    <interpGrp type="textYearLevel1_from">
      <interp></interp>
    </interpGrp>
    <interpGrp type="textYearLevel1_to">
      <interp></interp>
    </interpGrp>
    <interpGrp type="textMonthLevel1_from">
      <interp/>
    </interpGrp>
    <interpGrp type="textMonthLevel1_to">
      <interp/>
    </interpGrp>
    <interpGrp type="textDayLevel1_from">
      <interp/>
    </interpGrp>
    <interpGrp type="textDayLevel1_to">
      <interp/>
    </interpGrp>
    <interpGrp type="textYearLevel2_from">
      <interp>1300</interp>
    </interpGrp>
    <interpGrp type="textYearLevel2_to">
      <interp>1400</interp>
    </interpGrp>
    <interpGrp type="textYearLevel3_from">
      <interp></interp>
    </interpGrp>
    <interpGrp type="textYearLevel3_to">
      <interp/>
    </interpGrp>
    <interpGrp type="witnessLocalization_placeLevel1">
      <interp></interp>
    </interpGrp>
    <interpGrp type="witnessLocalization_countryLevel1">
      <interp></interp>
    </interpGrp>
    <interpGrp type="witnessLocalization_regionLevel1">
      <interp></interp>
    </interpGrp>
    <interpGrp type="witnessLocalization_kloekecodeLevel1">
      <interp></interp>
    </interpGrp>
    <interpGrp type="witnessLocalization_placeLevel2">
      <interp/>
    </interpGrp>
    <interpGrp type="witnessLocalization_provinceLevel2">
      <interp/>
    </interpGrp>
    <interpGrp type="witnessLocalization_countryLevel2">
      <interp>Nederland</interp>
      <interp>Belgi&#xEB;</interp>
      <interp>Frankrijk</interp>
      <interp>Duitsland</interp>
    </interpGrp>
    <interpGrp type="witnessLocalization_countryLevel3">
      <interp>Nederland</interp>
      <interp>Belgi&#xEB;</interp>
      <interp>Frankrijk</interp>
      <interp>Duitsland</interp>
    </interpGrp>
    <interpGrp type="witnessLocalization_regionLevel2">
      <interp/>
    </interpGrp>
    <interpGrp type="witnessLocalization_kloekecodeLevel2">
      <interp/>
    </interpGrp>
    <interpGrp type="textLocalization_placeLevel1">
      <interp></interp>
    </interpGrp>
    <interpGrp type="textLocalization_countryLevel1">
      <interp></interp>
    </interpGrp>
    <interpGrp type="textLocalization_regionLevel1">
      <interp></interp>
    </interpGrp>
    <interpGrp type="textLocalization_kloekecodeLevel1">
      <interp></interp>
    </interpGrp>
    <interpGrp type="textLocalization_placeLevel2">
      <interp/>
    </interpGrp>
    <interpGrp type="textLocalization_provinceLevel2">
      <interp/>
    </interpGrp>
    <interpGrp type="textLocalization_countryLevel2">
      <interp>Nederland</interp>
      <interp>Belgi&#xEB;</interp>
      <interp>Frankrijk</interp>
      <interp>Duitsland</interp>
    </interpGrp>
    <interpGrp type="textLocalization_countryLevel3">
      <interp>Nederland</interp>
      <interp>Belgi&#xEB;</interp>
      <interp>Frankrijk</interp>
      <interp>Duitsland</interp>
    </interpGrp>
    <interpGrp type="textLocalization_regionLevel2">
      <interp/>
    </interpGrp>
    <interpGrp type="textLocalization_kloekecodeLevel2">
      <interp/>
    </interpGrp>
    <interpGrp type="ingestTime">
      <interp></interp>
    </interpGrp>
    <interpGrp type="genreLevel1">
      <interp>geschiedenis</interp>
    </interpGrp>
    <interpGrp type="genreLevel2">
      <interp>geschiedenis</interp>
    </interpGrp>
    <interpGrp type="genreLevel3">
      <interp/>
    </interpGrp>
    <interpGrp type="subgenreLevel1">
      <interp>offici&#xEB;le documenten</interp>
    </interpGrp>
    <interpGrp type="subgenreLevel2">
      <interp>offici&#xEB;le documenten</interp>
    </interpGrp>
    <interpGrp type="subgenreLevel3">
      <interp/>
    </interpGrp>
    <interpGrp type="factualityLevel1">
      <interp>non-fictie</interp>
    </interpGrp>
    <interpGrp type="factualityLevel2">
      <interp>non-fictie</interp>
    </interpGrp>
    <interpGrp type="factualityLevel3">
      <interp/>
    </interpGrp>
    <interpGrp type="statusTextLevel1">
      <interp>geen vertaling</interp>
    </interpGrp>
    <interpGrp type="statusTextLevel2">
      <interp>geen vertaling</interp>
    </interpGrp>
    <interpGrp type="statusTextLevel3">
      <interp/>
    </interpGrp>
    <interpGrp type="primaryLanguageLevel1">
      <interp>nld</interp>
    </interpGrp>
    <interpGrp type="primaryLanguageLevel2">
      <interp>nld</interp>
    </interpGrp>
    <interpGrp type="primaryLanguageLevel3">
      <interp>nld</interp>
    </interpGrp>
    <interpGrp type="ipr">
      <interp>extern</interp>
    </interpGrp>
    <interpGrp type="copyrightHolder">
      <interp>Pieter van Reenen</interp>
      <interp>Maaike Mulder</interp>
    </interpGrp>
    <interpGrp type="copyrightOwner">
      <interp>Pieter van Reenen</interp>
      <interp>Maaike Mulder</interp>
    </interpGrp>
    <interpGrp type="corpusProvenance">
      <interp>CRM</interp>
    </interpGrp>
    <interpGrp type="biblScope_pageLevel1">
      <interp/>
    </interpGrp>
    <interpGrp type="biblScope_pageLevel2">
      <interp/>
    </interpGrp>
    <interpGrp type="biblScope_volumeLevel1">
      <interp/>
    </interpGrp>
    <interpGrp type="biblScope_volumeLevel2">
      <interp/>
    </interpGrp>
    <interpGrp type="processingMethod">
      <interp>digitized</interp>
      <interp>born digital</interp>
    </interpGrp>
  </bibl>
}
