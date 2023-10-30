package corpusprocessing.CLVN

object Settings {
  val atHome = false
  val ditAtWork = "/mnt/Projecten/Nederlab/DiT-corpusproject-update/"
  val ditAtHome = "/mnt/DiskStation/homes/jesse/work/DiT-corpusproject/"
  val ditDirectory:String = if (atHome) ditAtHome else ditAtWork

  val ditconfig = database.Configuration(
    name = "ditmeta",
    server = "svowdb02",
    database = "ditmetadata",
    user = "postgres",
    password = "inl")

  val ditDevconfig = database.Configuration(
    name = "ditmeta",
    server = "svowdb02",
    database = "ditmetadata_dev",
    user = "postgres",
    password = "inl")

  val leuvenDir:String = ditDirectory + "10_Leuven/Jesse/"
  val peterDir:String = ditDirectory + "11_Peter_van_Os/Jesse/"
  val gelderDir:String = ditDirectory + "07_Gelders_Archief/Jesse/"
  val delflandDir:String = ditDirectory + "01_Archief_Delfland/Jesse/"
  val elburgDir:String = ditDirectory + "02_Archief_Elburg/Jesse/"
  val enschedeDir:String = ditDirectory + "04_ArchiefEnschede/Jesse/"
  val seyger1Dir:String = ditDirectory + "12_Seyger-metmetadata/Seyger-1/JESSE/"
  val seyger2Dir:String = ditDirectory + "12_Seyger-metmetadata/Seyger-2/JESSE/"
  val ditToevoegingenDir: String = ditDirectory + "06_DiT-Corpus/DiT-corpusvoorJesse/Toevoegingen/"
  val zutphenBase: String = ditDirectory + "05_ArchiefZutphen/"
  val alkmaarBase: String = ditDirectory + "03_ArchiefAlkmaar/"
  val alkmaarDir = alkmaarBase + "Jesse/"
  val gemertDir = ditDirectory + "08_HeemkundekringGemert/Jesse/"
  val bredeVoortDir = ditDirectory +  "09_HofboekBredevoort/Jesse/"
  val zutphenSubsWithPlainText = List("Brieven_1460+1461/Jesse/", "Brieven_1571-1580/Jesse/", "Markeboek_Empe/Jesse/", "OudeConvent/Jesse/",
    "Zutphen_Vredeboek/Jesse/")
}

/*

Brieven 1460+1461
Brieven 1571-1580
Codes Zutphen.doc
Markeboek Empe
NIET-TEJONG-Voorstonden_Brummen
OudeConvent
Thumbs.db
Zutphen1571
Zutphen1572
Zutphen1573
Zutphen1574
Zutphen1575
Zutphen1577
Zutphen1578
Zutphen1580
Zutphen Vredeboek

 */