package corpusprocessing.UDCorpora

object Families {

  case class Table(header: Seq[String], rows: Seq[Seq[String]]) {

    println("Init table: "  + header)
    lazy val f2n = header.zipWithIndex.toMap


    def get(i: Int) = { val r = rows(i);

       r.indices.take(header.size).map(j => header(j) -> r(j)).toMap }
    val rowsAsMaps =  { val z = rows.indices.map(get); println("maps done") ; z }

    def find(name: String, value: String)  = rowsAsMaps.filter(m => m.contains(name) && m(name) == value)
  }
  def readTable(f: String)  = {
    val rows = io.Source.fromFile(f).getLines().map(s => s.split(",").toList).toList
    Table(rows.head, rows.tail)
  }

  lazy val languageTable = readTable("data/Glottolog/languages.csv")
  lazy val classificationTable = readTable("data/Glottolog/classification.csv")


  def getClassification(langCode: String) = {
    val langInfo = languageTable.find("ISO639P3code", langCode).headOption
    langInfo.flatMap(info => {
      val cls: Option[Map[String, String]] = classificationTable.find("Language_ID", info("Glottocode")).filter(m => m("Parameter_ID") == "classification").headOption
      cls.map(row => {
        val z = row("Value")
        z.split("/").flatMap(Glottocode => {
          val classInfo = languageTable.find("Glottocode", Glottocode).headOption
          classInfo.map(_("Name"))
        })
      })
    }).getOrElse(Array()).toList
  }
  def main(args: Array[String])  = {
     println(getClassification("dut"))
     languageTable.find("Level", "language").foreach(m => {
       val iso = m("ISO639P3code")
       val classification = getClassification(iso);
       println(m("Name") + "  "  + classification)
     })
  }
}


/*

languages.csv

ID,Name,Macroarea,Latitude,Longitude,Glottocode,ISO639P3code,Level,Countries,Family_ID,Language_ID,Closest_ISO369P3code,First_Year_Of_Documentation,Last_Year_Of_Documentation,Is_Isolate

dutc1256,Dutch,Eurasia,52.0,5.0,dutc1256,nld,language,AW;BE;BQ;BR;CW;DE;GF;GY;NL;SR;SX,indo1319,,nld,,,false
languages.csv:indo1319,Indo-European,,41.88782235237204,32.94360183299437,indo1319,,family,,,,,,,

values.csv
ID,Language_ID,Parameter_ID,Value,Code_ID,Comment,Source,codeReference
dutc1256-classification,dutc1256,classification,indo1319/clas1257/germ1287/nort3152/west2793/macr1270/midd1347/mode1257/glob1241,,,,

 */