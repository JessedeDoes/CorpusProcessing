package corpusprocessing.UDCorpora


/*
ID,Name,Macroarea,Latitude,Longitude,Glottocode,ISO639P3code,Level,Countries,Family_ID,Language_ID,Closest_ISO369P3code,First_Year_Of_Documentation,Last_Year_Of_Documentation,Is_Isolate
 */
case class LangInfo(m: Map[String, String]) {
  lazy val name = m("Name")
  lazy val level = m("Level")
  lazy val id = m("ID")
  lazy val macroarea = m("Macroarea")
  lazy val countries = m.getOrElse("Countries", "").split(";").toList

  override def toString() = s"($name,$level,$countries)"
}

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
          val classInfo: Option[Map[String, String]] = languageTable.find("Glottocode", Glottocode).headOption
          classInfo.map(LangInfo)
        })
      })
    }).getOrElse(Array()).toList
  }

  import scala.xml._
  def makeTree(l: Seq[List[LangInfo]]): Elem  = {
    val groups = l.groupBy(_.head).mapValues(_.map(_.tail))
    val grouptags =   {groups.map(g =>
      <category type={g._1.level}>
        <catDesc>{g._1.name}</catDesc>
        {makeTree(g._2.filter(_.nonEmpty)).filter(_.label != "none")}
      </category>)}
    if (grouptags.size > 1)
    <taxonomy>
      {grouptags}
    </taxonomy>
    else grouptags.headOption.getOrElse(<none/>)
  }

 def testCodes(): Unit = {
   val codes = io.Source.fromFile("data/Glottolog/codes.txt").getLines().toList
   val classifications: Seq[(String, List[LangInfo])] = codes.map(c => {
     val mapped = languageCodes.two2three(c)
     val cls = getClassification(mapped)
     if (cls.isEmpty) { println(s"$c->$mapped: $cls"); c -> cls }
     else {
       val currentInfo = LangInfo(languageTable.find("ISO639P3code", mapped).head)
       c -> (cls ++ List(currentInfo))
     }
   }).filter(_._2.nonEmpty)
   val tree = makeTree(classifications.map(_._2))
   val pretty = new scala.xml.PrettyPrinter(200,4)
   println(pretty.format(tree))

 }

  def main(args: Array[String])  = {
    testCodes()
    System.exit(0)
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

No match:

ar->ara: List()
az->aze: List()
et->est: List()
eu->eus: List()
fa->fas: List()
gn->grn: List()
qaf->qaf: List()
qfn->qfn: List()
qpm->qpm: List()
qtd->qtd: List()
qte->qte: List()
zh->zho: List()

 */