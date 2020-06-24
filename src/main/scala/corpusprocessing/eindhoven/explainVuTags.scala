package corpusprocessing.eindhoven

import scala.util.{Failure, Success, Try}
object explainVuTags {

  val tagMapping = io.Source.fromFile("data/vu.taginfo.csv").getLines().map(l => l.split("\\s+")).filter(_.size >=2)
    .map(l => l(0) -> l(1).replaceAll("\\s.*","")).toMap

  def findVuPos(pos: String):(Int,Int,Int) =
  {
    val vuPos0:Int = "([0-9]{3})".r.findFirstMatchIn(pos).map(m => m.group(1)).getOrElse("999").toInt

    val vuPos = vuPos0 % 1000
    val vuMainPos = (vuPos - vuPos % 100) / 100
    val vuSub1 = vuPos - 100 * vuMainPos
    val vuSubPos = (vuSub1 - vuSub1 % 10) / 10
    val vuSubSubPos = vuPos - 100 * vuMainPos - 10 * vuSubPos

    (vuMainPos, vuSubPos, vuSubSubPos)
  }


  def basicMap = Map(0 -> "znw", 1 -> "bnw", 2 -> "ww", 3 -> "vnw", 4 -> "vnw", 5 -> "bw", 6 -> "vz", 7 -> "vg", 8 -> "tsw", 9 -> "res")

  val tweedeCijferMap = Map(
    0 -> Map(0 -> "soort,nominaal gebruikt", 1 -> "eigennaam", 2 -> "adjectivisch", 8 -> "interjectivisch", 9 -> "zelfnoemfunctie"),

    1 -> Map(0 -> "gewoon (predicatief of attributief)", 2 -> "gesubstantiveerd", 5 -> "adverbiaal", 8 -> "interjectivisch"),

    2 -> Map(0 -> "intrans", 1 -> "trans", 2 -> "refl", 3 -> "hulp of koppelwerkwoord",
      4 -> "pv, intrans", 5 -> "pv, trans", 6 -> "pv, refl", 7 -> "pv, hulp of koppelwerkwoord"),

    3 -> Map(0 -> "persoonlijk",
      2 -> "bezittelijk, zelfstandig", 3 -> "bezittelijk, bijvoeglijk",
      4 -> "reflexief, zelfstandig", 5 -> "reflexief, bijvoeglijk",
      6 -> "aanwijzend, zelfstandig", 7 -> "aanwijzend, bijvoeglijk"),

    4 -> Map(
      0 -> "vragend. zelfstandig", 1 -> "vragend, bijvoeglijk",
      2 -> "betrekkelijk, zelfstandig", 3 -> "betrekkelijk, bijvoeglijk",
      4 -> "onbepaald, zelfstandig", 5 -> "onbepaald, bijvoeglijk",
      6 -> "hoofd, zelfstandig", 7 -> "hoofd, bijvoeglijk",
      8 -> "rang, zelfstandig", 9 -> "rang, bijvoeglijk",
    ),

    5 -> Map(
      0 -> "gewoon",
      1 -> "aanwijzend en onbepaald",
      3 -> "betrekkelijk",
      4 -> "voornaamwoordelijk",
      5 -> "vragend voornaamwoordelijk",
      6 -> "betrekkelijk voornaamwoordelijk",
      8 -> "interjectivisch"
    ),

    6 -> Map(
      0 -> "gewoon",
      1 -> "deel voorn. bijw.",
      2 -> "deel scheidbaar ww.",
      3 -> "deel circumpositie",
      4 -> "achterzetsel",
      5 -> "te of met te-constructie",
      6 -> "met onderschikkende bijzin ingeleid door voegwoord"
    ),

    6 -> Map(
      0 -> "nevenschikkend",
      1 -> "onderschikkeld",
      2 -> "vergelijkend",
      3 -> "al (met afwijkende volgorde in bijzin)",
      4 -> "deel nevenschikkende voegwoordgroep"
    ),

    8 -> Map(
      0 -> "echt",
      1 -> "onomatopeeen"
    ),

    )

    val derdeCijferMap =  Map(


    "most" -> Map(0 -> "basisvorm",
      1->"meervoud",
      2->"genitief",
      3->"overige buiging",
      4->"comparatief onverbogen",
      5 -> "comparatief genitief",
      6 ->"comparatief overige buiging",
      7-> "superlatief onverbogen",
      8 -> "superlatief genitief",
      9 -> "superlatief overige buiging"
    ),
    "wwnietfiniet" -> Map(
      0 -> "infinitief,verbaal",
      1 -> "infinitief, nominaal",
      2 -> "od, onverbogen",
      3 -> "od, verbogen",
      4 -> "od, meervoud",
      5 -> "od, bijwoordelijk",
      6 -> "vd, onverbogen",
      7 -> "vd, verbogen",
      8 -> "vd, meervoud",
      9 -> "vd, bijwoordelijk",
    ),
      "wwfiniet" -> Map(
        1 -> "1 ev tgw",  2 -> "od, onverbogen",
      3 -> "od, verbogen",
      4 -> "od, meervoud",
      5 -> "od, bijwoordelijk",
        2 -> "2 ev tgw",
        3 -> "3 ev tgw",
        4 -> "mv tgw",
        5 -> "sg verl",
        6 -> "pl verl",
        7 -> "imperatief zonder pronomen",
        8 -> "imperatief met pronomen",
        9 -> "overige vormen"
      ),
      "res" -> Map(
        0 -> "anderstalig",
        1 -> "niet-lexicale verbindingselementen",
        2 -> "rekenkundige symbolen",
        3 -> "overige symbolen",
        8 -> "niet tot de steekproef behorende elementen"
      )
    )

  def woordsoort(een: Int, twee: Int): String = {
    val ws = basicMap(een)
    if (een == 4 && twee >= 6) "tw" else ws
  }

  def explainAll() =
  {
    //(0 to 9).flatMap(i => (0 to 9).flatMap(j => (0 to 9).map(k => explain(i,j, k)))).foreach(println)
    tagMapping.toList.sortBy(_._1).foreach(
      {case (vu, cgn) =>
          val (een,twee,drie) = findVuPos(vu)
          val expl = explain(een,twee,drie)
          println(s"$vu\t$cgn\t$expl")
      }
    )
  }

  def explain(een: Int, twee: Int, drie: Int) = {
     val ws = woordsoort(een,twee)
     val typ = Try (tweedeCijferMap(een)(twee)) match {
       case Success(s) => s
       case Failure(_) => ""
     }

    val driekey = if (een == 9) "res" else if (een==2 && twee < 4) "wwnietfiniet" else if (een ==2 && twee >= 4) "wwfiniet" else "most"

    val drietje = Try (derdeCijferMap(driekey)(drie)) match {
      case Success(s) => s
      case Failure(_) => ""
    }

    s"$ws $typ $drietje".replaceAll("\\s+", " ")
  }

  def main(args: Array[String]): Unit = {
    explainAll()
  }

}
