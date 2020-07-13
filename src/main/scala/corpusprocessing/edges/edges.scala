package corpusprocessing.edges
import scala.xml._
object edges {
   val metadata = Map(
     "Delftse_bijbel" -> Map(
       "date" -> "1477",
       "title" -> "Delftse bijbel"
     ),
     "DeuxAes" -> Map(
       "date" -> "1562",
       "title" -> "Deux Aes"
     ),
    "Lutherse_bijbel" -> Map(
      "date" -> "1648",
      "title" -> "Lutherse bijbel"
    ),
     "SV1657_xml_dbnl" -> Map(
    "date" -> "1657",
    "title" -> "Statenvertaling 1657"
    ),
     "Leuvense_bijbel" -> Map(
       "date" -> "1548",
       "title" -> "Statenvertaling 1657"
     )
   )

  def interp(n: String,v: String)  = <interpGrp type={n}><interp>{v}</interp></interpGrp>

  def meta(f: String) = {
    val m = metadata(f)
    def interp(n: String,v: String)  = { <interpGrp type={n}><interp>{m(v)}</interp></interpGrp> }

    <bibl>
      {interp("titleLevel2", "title")}
      {interp("witnessYearLevel2_from", "date")}
      {interp("witnessYearLevel2_to", "date")}
      {interp("witnessYearLevel2_from", "date")}
      {interp("witnessYearLevel2_to", "date")}
    </bibl>
  }
  def main(args: Array[String]): Unit = {

  }
}


/*
Delftse_bijbel
DeuxAes
Leuvense_bijbel
Lutherse_bijbel
SV1657_xml_dbnl

 */