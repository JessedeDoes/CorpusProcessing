package corpusprocessing.CLVN

import enhanceMetadata.PostProccesing
import seyger2Patch.{defaultTransfer, setGlobal, someDefaults}

object leuvenPatches {

}

object leuven_1430_1439_patch extends PostProccesing("1-Leuven", Some("1430-1439"))
{
  override def apply():Unit = {
    someDefaults

    setGlobal(List(


      "authorLevel1" -> "",
      "authorLevel2" -> "Gerard Seyger",
      "editorLevel1" -> "Gerard Seyger",

      "titleLevel2" -> "De geboorte en bloei van het twents als schrijftaal in de late middeleeuwen",
      "subTitleLevel2" -> "Corpus van 14e en 15e eeuwse oorkonden in de volkstaal, uit de steden Almelo, Enschede, Oldenzaal en Ootmarsum en van ambtman en rentmeester in Twenthe. Bijdrage tot een bronnenboek van Twenthe",

      "biblScope_pageLevel2" -> "1-24",

      "genreLevel1" -> "geschiedenis",
      "genreLevel2" -> "geschiedenis",

      "subGenreLevel1" -> "officiële documenten",
      "subGenreLevel2"-> "officiële documenten",


      "witnessYearLevel2_from" -> "1430",
      "witnessYearLevel2_to" -> "1440",



      "pubYearLevel2_from" -> "1999",
      "pubYearLevel2_to" -> "2011",


      "localization_regionLevel1" -> "Vlaanderen",
      "localization_regionLevel2" -> "Twente",

      "localization_provinceLevel1" -> "Overijssel",
      "localization_provinceLevel2" -> "Overijssel",

      "localization_countryLevel1" -> "Nederland",
      "localization_countryLevel2" -> "Nederland",

      "categoryLevel1" -> "uitgave",
      "categoryLevel2" -> "uitgave",


      "editionLevel1" -> "1",
      "editionLevel2" -> "1",
      "mediumLevel1" -> "digitale uitgave",
      "mediumLevel2" -> "digitale uitgave",

      "resourceURILevel1" -> "http://www.diachronie.nl/corpora/pdf/CORPUS.DISSERTATIE.GERARDSEYGER.WWW.diachronie.nl.definitief.pdf",
      "resourceURILevel2" -> "http://www.diachronie.nl/corpora/pdf/CORPUS.DISSERTATIE.GERARDSEYGER.WWW.diachronie.nl.definitief.pdf",

      //"publisherLevel1" -> "Stadsarchief Enschede",
      //"publisherLevel2" -> "Stadsarchief Enschede",

      "placePublicationLevel2" -> "Oldenzaal",
      "pubYearLevel1_from" -> "2009",
      "pubYearLevel1_to" -> "2017",
      "pubYearLevel2_from" -> "2009",
      "pubYearLevel2_to" -> "2007",

      "primaryLanguageLevel1" -> "nld",
      "primaryLanguageLevel2" -> "nld",


      "copyrightHolder" -> "Stadsarchief Enschede",
      "copyrightOwner" -> "Stadsarchief Enschede",

      "notesStmt" -> "<p>Corpus van 14e en 15e eeuwse oorkonden in de volkstaal, uit de steden Almelo, Enschede, Oldenzaal en Ootmarsum en van ambtman en rentmeester in Twenthe. Bijdrage tot een bronnenboek van Twenthe.</p>"
    ))


    defaultTransfer
  }
}