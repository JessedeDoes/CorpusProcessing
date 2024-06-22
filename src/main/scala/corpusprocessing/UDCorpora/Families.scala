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
  lazy val code = m("ISO639P3code")
  override def toString() = s"($name,$code,$id,$level,$countries)"
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

  def getClassificationMapped(langCode: String)  = {
    val mapped0 = languageCodes.two2three(langCode)
    val mapped = replaceMents.getOrElse(mapped0, mapped0)
    val cls = getClassification(mapped)
    cls
  }

  import scala.xml._
  def makeTree(l: Seq[List[LangInfo]],level: Int=0): Elem  = {
    val groups = l.groupBy(_.head).mapValues(_.map(_.tail))
    val grouptags =   {groups.map(g =>
      <category datcat="https://iso639-3.sil.org/code/" valueDatCat={g._1.code} level={level.toString} xml:id={g._1.id} type={g._1.level}>
        <catDesc>{g._1.name}</catDesc>
        {makeTree(g._2.filter(_.nonEmpty),level+1).filter(_.label != "none")}
      </category>)}
    if (grouptags.size > 1)
    <taxonomy>
      {grouptags}
    </taxonomy>
    else grouptags.headOption.getOrElse(<none/>)
  }
  val replaceMents = Map("ara" -> "acm", "aze" -> "azb", "est" -> "ekk", "__eus" -> "baq", "fas" -> "pes", "grn" -> "gnw", "zho" -> "cmn")
 def testCodes(): Unit = {
   val codes = io.Source.fromFile("data/Glottolog/codes.txt").getLines().toList
   val classifications: Seq[(String, List[LangInfo])] = codes.map(c => {


     val mapped0 = languageCodes.two2three(c)
     val mapped = replaceMents.getOrElse(mapped0, mapped0)
     val cls = getClassification(mapped)



     if (cls.isEmpty) { println(s"$c->$mapped: $cls"); c -> cls }
     else {
       val currentInfo0 = LangInfo(languageTable.find("ISO639P3code", mapped).head)
       val currentInfo = if (replaceMents.contains(mapped0)) {
         println(s"Using mapping for two=$c,three=$mapped0,threeMapped=$mapped: $cls")
         val m1 = currentInfo0.m.map{case ("ISO639P3code", _) => ("ISO639P3code" -> mapped0); case x => x }
         println("Put back:" + m1("ISO639P3code"))
         val putCodeBack = currentInfo0.copy(m =  m1)
         println(putCodeBack)
         putCodeBack
       } else currentInfo0

       c -> (cls ++ List(currentInfo))
     }
   }).filter(_._2.nonEmpty)

   val tree = makeTree(classifications.map(_._2))
   val pretty = new scala.xml.PrettyPrinter(200,4)
   println(pretty.format(tree))
   val pw = new java.io.PrintWriter("/tmp/langtax.xml")
   pw.println(pretty.format(tree))
   pw.close()
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
Identifier	Reference Name	Status
aao	Algerian Saharan Arabic	Active
abh	Tajiki Arabic	Active
abv	Baharna Arabic	Active
acm	Mesopotamian Arabic	Active
acq	Ta'izzi-Adeni Arabic	Active
acw	Hijazi Arabic	Active
acx	Omani Arabic	Active
acy	Cypriot Arabic	Active
adf	Dhofari Arabic	Active
aeb	Tunisian Arabic	Active
aec	Saidi Arabic	Active
afb	Gulf Arabic	Active
ajp	South Levantine Arabic	Deprecated
apc	Levantine Arabic	Active
apd	Sudanese Arabic	Active
arb	Standard Arabic	Active
arq	Algerian Arabic	Active
ars	Najdi Arabic	Active
ary	Moroccan Arabic	Active
arz	Egyptian Arabic	Active
auz	Uzbeki Arabic	Active
avl	Eastern Egyptian Bedawi Arabic	Active
ayh	Hadrami Arabic	Active
ayl	Libyan Arabic	Active
ayn	Sanaani Arabic	Active
ayp	North Mesopotamian Arabic	Active
bbz	Babalia Creole Arabic	Deprecated
pga	Sudanese Creole Arabic	Active
shu	Chadian Arabic	Active
ssh	Shihhi Arabic	Active

val replaceMents = Map("ara" -> "acm", "aze" -> "azb", "est" -> "ekk", "eus" -> "baq", "fas" -> "pes", "grn" -> "gnw", "zho" -> "cmn")


az->aze: List()
Identifier	Reference Name	Status
azb	South Azerbaijani	Active
azj	North Azerbaijani	Active

et->est: List()
Identifier	Reference Name	Status
ekk	Standard Estonian	Active
vro	Võro	Active

eu->eus: List() --? baq

fa->fas: List()
Identifier	Reference Name	Status
pes	Iranian Persian	Active
prs	Dari	Active

gn->grn: List()  Guarani [grn]
gnw	Western Bolivian Guaraní	Active
gug	Paraguayan Guaraní	Active
gui	Eastern Bolivian Guaraní	Active
gun	Mbyá Guaraní	Active
nhd	Chiripá	Active

qaf->qaf: List() Reserved for local use
qfn->qfn: List() Reserved for local use
qpm->qpm: List() Reserved for local use
qtd->qtd: List()  Reserved for local use
qte->qte: List() Reserved for local use
zh->zho: List() https://iso639-3.sil.org/code/zho  maan er cmn van

 */