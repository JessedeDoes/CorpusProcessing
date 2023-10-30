package corpusprocessing.CLVN

import enhanceMetadata.PostProccesing

object patches {

}

object delflandPatch extends PostProccesing("01_Archief", None)
{
  override def apply():Unit = {
    someDefaults

    setGlobal(List(
      "biblScope_pageLevel2" -> "1-24",
      "genreLevel1" -> "geschiedenis",
      "genreLevel2" -> "geschiedenis",
      "subGenreLevel1" -> "officiële documenten",
      "subGenreLevel2"-> "officiële documenten",

      "textDayLevel2_from" -> "21",
      "textDayLevel2_to" -> "31",
      "textMonthLevel2_from" -> "11",
      "textMonthLevel2_to" -> "01",
      "textYearLevel2_from" -> "1529",
      "textYearLevel2_to" -> "1533",

      "witnessDayLevel2_from" -> "21",
      "witnessDayLevel2_to" -> "31",
      "witnessMonthLevel2_from" -> "11",
      "witnessMonthLevel2_to" -> "01",
      "witnessYearLevel2_from" -> "1529",
      "witnessYearLevel2_to" -> "1533",

      "localization_regionLevel1" -> "Holland|Delfland",
      "localization_regionLevel2" -> "Holland|Delfland",


      "localization_countryLevel1" -> "Nederland",
      "localization_countryLevel2" -> "Nederland",

      "categoryLevel1" -> "transcriptie",
      "categoryLevel2" -> "transcriptie",


      "editionLevel1" -> "1",
      "editionLevel2" -> "1",
      "mediumLevel1" -> "digitale uitgave",
      "mediumLevel2" -> "digitale uitgave",

      "resourceURILevel1" -> "https://www.hhdelfland.nl/over-ons/pdfs/oudarchief1622.pdf",
      "resourceURILevel2" -> "https://www.hhdelfland.nl/over-ons/pdfs/oudarchief1622.pdf",

      "publisherLevel1" -> "Hoogheemraadschap van Delfland",
      "publisherLevel2" -> "Hoogheemraadschap van Delfland",

      "pubYearLevel1_from" -> "2014",
      "pubYearLevel1_to" -> "2014",
      "pubYearLevel2_from" -> "2014",
      "pubYearLevel2_to" -> "2014",

      "primaryLanguageLevel1" -> "nld",
      "primaryLanguageLevel2" -> "nld",


      "copyrightHolder" -> "Hoogheemraadschap van Delfland",
      "copyrightOwner" -> "Hoogheemraadschap van Delfland",

      "notesStmt" -> "<note><p>Transcriptie van RECHTSGEDINGENBOEK 1529 – 1539, gedeelte 21 nov. 1529 – 31 januari 1533, Oud Archief Delfland, inv.nr. 1622, versie 1 oktober 2014</p></note>"
    ))

    perRecord(List(
      ("1532jDelfland", "sourceId", "1533jDelfland"),
      ("1532jDelfland", "witnessYearLevel1_from", "1533"),
      ("1532jDelfland", "witnessYearLevel1_to", "1533"),
      ("1532jDelfland", "titleLevel1", "Delfland 1533-01-31")
    ))

    defaultTransfer
  }
}

object elburgPatch extends PostProccesing("02_Archief", None)
{
  override def apply():Unit = {
    someDefaults

    setGlobal(List(
      "editorLevel1" -> "E. Kranenburg-van der Beek",
      "editorLevel2" -> "E. Kranenburg-van der Beek",
      //"biblScope_pageLevel2" -> "1-24",
      "genreLevel1" -> "geschiedenis",
      "genreLevel2" -> "geschiedenis",
      "subGenreLevel1" -> "officiële documenten",
      "subGenreLevel2"-> "officiële documenten",

      "textDayLevel2_from" -> "14",
      "textDayLevel2_to" -> "01",
      "textMonthLevel2_from" -> "06",
      "textMonthLevel2_to" -> "12",
      "textYearLevel2_from" -> "1563",
      "textYearLevel2_to" -> "1596",

      "witnessDayLevel2_from" -> "14",
      "witnessDayLevel2_to" -> "01",
      "witnessMonthLevel2_from" -> "06",
      "witnessMonthLevel2_to" -> "12",
      "witnessYearLevel2_from" -> "1563",
      "witnessYearLevel2_to" -> "1596",

      "localization_placeLevel2" -> "Elburg",
      "localization_regionLevel1" -> "Veluwe",
      "localization_regionLevel2" -> "Veluwe",

      "localization_countryLevel1" -> "Nederland",
      "localization_countryLevel2" -> "Nederland",

      "categoryLevel1" -> "uitgave",
      "categoryLevel2" -> "uitgave",

      "editionLevel1" -> "1",
      "editionLevel2" -> "1",
      "mediumLevel1" -> "digitale uitgave",
      "mediumLevel2" -> "digitale uitgave",

      "resourceURILevel1" -> "http://www.streekarchivariaat.nl/collecties?mivast=434&miadt=434&mizig=210&miview=inv2&milang=nl&micols=1&mires=0&micode=1003&mizk_alle=oorvedenboek",
      "resourceURILevel2" -> "http://www.streekarchivariaat.nl/collecties?mivast=434&miadt=434&mizig=210&miview=inv2&milang=nl&micols=1&mires=0&micode=1003&mizk_alle=oorvedenboek",

      "publisherLevel1" -> "Streekarchivariaat Noordwest-Veluwe",
      "publisherLevel2" -> "Streekarchivariaat Noordwest-Veluwe",

      "pubYearLevel1_from" -> "2000",
      "pubYearLevel1_to" -> "2000",
      "pubYearLevel2_from" -> "2000",
      "pubYearLevel2_to" -> "2000",

      "primaryLanguageLevel1" -> "nld",
      "primaryLanguageLevel2" -> "nld",


      "copyrightHolder" -> "Streekarchivariaat Noordwest-Veluwe",
      "copyrightOwner" -> "Streekarchivariaat Noordwest-Veluwe"

      // "notesStmt" -> "<note><p>Transcriptie van RECHTSGEDINGENBOEK 1529 – 1539, gedeelte 21 nov. 1529 – 31 januari 1533, Oud Archief Delfland, inv.nr. 1622,versie 1 oktober 2014</p></note>"
    ))


    defaultTransfer
  }
}

object enschedePatch extends PostProccesing("04_ArchiefEnschede", None)
{
  override def apply():Unit = {
    someDefaults

    setGlobal(List(

      "titleLevel2" -> "Processen stadgericht Enschede, nummer 73, boek 01",
      "biblScope_pageLevel2" -> "1-24",
      "genreLevel1" -> "geschiedenis",
      "genreLevel2" -> "geschiedenis",
      "subGenreLevel1" -> "officiële documenten",
      "subGenreLevel2"-> "officiële documenten",

      "witnessYearLevel2_from" -> "1581",
      "witnessYearLevel2_to" -> "1596",

      "localization_regionLevel1" -> "Twente",
      "localization_regionLevel2" -> "Twente",

      "localization_countryLevel1" -> "Nederland",
      "localization_countryLevel2" -> "Nederland",

      "categoryLevel1" -> "uitgave",
      "categoryLevel2" -> "uitgave",


      "editionLevel1" -> "1",
      "editionLevel2" -> "1",
      "mediumLevel1" -> "digitale uitgave",
      "mediumLevel2" -> "digitale uitgave",

      "resourceURILevel1" -> "http://stadsarchief.enschede.nl/bestanden/Stad_proces_73_-_1.pdf/",
      "resourceURILevel2" -> "http://stadsarchief.enschede.nl/bestanden/Stad_proces_73_-_1.pdf/",

      "publisherLevel1" -> "Stadsarchief Enschede",
      "publisherLevel2" -> "Stadsarchief Enschede",

      "pubYearLevel1_from" -> "?",
      "pubYearLevel1_to" -> "?",
      "pubYearLevel2_from" -> "?",
      "pubYearLevel2_to" -> "?",

      "primaryLanguageLevel1" -> "nld",
      "primaryLanguageLevel2" -> "nld",


      "copyrightHolder" -> "Stadsarchief Enschede",
      "copyrightOwner" -> "Stadsarchief Enschede"
    ))


    defaultTransfer
  }
}


object seyger1Patch extends PostProccesing("12_Seyger", Some("Seyger-1"))
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
      "genreLevel2" -> "taalkunde",

      "subGenreLevel1" -> "officiële documenten",
      "subGenreLevel2"-> "proefschrift",


      "witnessYearLevel2_from" -> "1400",
      "witnessYearLevel2_to" -> "1500",


      "pubYearLevel2_from" -> "1999",
      "pubYearLevel2_to" -> "2011",


      "localization_regionLevel1" -> "Twente",
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
      "pubYearLevel1_from" -> "?",
      "pubYearLevel1_to" -> "?",
      "pubYearLevel2_from" -> "?",
      "pubYearLevel2_to" -> "?",

      "primaryLanguageLevel1" -> "nld",
      "primaryLanguageLevel2" -> "nld",


      "copyrightHolder" -> "Stadsarchief Enschede",
      "copyrightOwner" -> "Stadsarchief Enschede",

      "notesStmt" -> "<p> Uitsluitend originele oorkonden, die exact gedateerd en gelocaliseerd kunnen worden en die zijn geschreven in de vier bovengenoemde steden, worden in dit corpus opgenomen.</p>"
    ))


    defaultTransfer
  }
}

object seyger2Patch extends PostProccesing("12_Seyger", Some("Seyger-2"))
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
      "genreLevel2" -> "taalkunde",

      "subGenreLevel1" -> "officiële documenten",
      "subGenreLevel2"-> "proefschrift",


      "textDayLevel2_from" -> "16",
      "textDayLevel2_to" -> "06",
      "textMonthLevel2_from" -> "09",
      "textMonthLevel2_to" -> "09",
      "textYearLevel2_from" -> "2005",
      "textYearLevel2_to" -> "2005",


      "pubYearLevel2_from" -> "1999",
      "pubYearLevel2_to" -> "2011",


      "localization_regionLevel1" -> "Twente",
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
      "pubYearLevel1_from" -> "?",
      "pubYearLevel1_to" -> "?",
      "pubYearLevel2_from" -> "?",
      "pubYearLevel2_to" -> "?",

      "primaryLanguageLevel1" -> "nld",
      "primaryLanguageLevel2" -> "nld",


      "copyrightHolder" -> "Stadsarchief Enschede",
      "copyrightOwner" -> "Stadsarchief Enschede",

      "notesStmt" -> "<p>Corpus van 14e en 15e eeuwse oorkonden in de volkstaal, uit de steden Almelo, Enschede, Oldenzaal en Ootmarsum en van ambtman en rentmeester in Twenthe. Bijdrage tot een bronnenboek van Twenthe.</p>"
    ))


    defaultTransfer
  }
}