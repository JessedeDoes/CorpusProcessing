package CRM

import Settings._


case class Location(kloeke_code1: String, kloeke_code_oud: String, plaats: String, gemeentecode_2007: String, gemeente: String,
                    streek: String, provincie: String, postcode: String, land: String, rd_x: String, rd_y: String, lat: String, lng: String,
                    topocode: String, std_spelling: String, volgnr: String, asRegion: Boolean = false)
{
  import Location._
  lazy val (landnaam,provincienaam) = provinciecodes(provincie)
  lazy val provNaamTitel = if (provincienaam.nonEmpty) provincienaam else landnaam
}

object Location
{
  private val kloekeCodes = Location.allKloekeCodes(dir + "kloeke_cumul.csv")

  val kloekeByCode:Map[String, Location] = kloekeCodes.groupBy(_.kloeke_code1.trim).mapValues(_.head)

  val provinciecodes: Map[String,(String,String)] = Map (
    "Be" -> ("België",""),
    "BeAnt" -> ("België","Antwerpen"),
    "BeBr" -> ("België","Brabant"),
    "BeHe" -> ("België","Henegouwen"),
    "Belb" -> ("België","Limburg"),
    "BeLb" -> ("België","Limburg"),
    "BeLb, Luik" -> ("België","Limburg|Luik"),
    "BeLu" -> ("België","Luik"), /* ?? */
    "Be, Luik" -> ("België","Luik"),
    "BeOv" -> ("België","Oost-Vlaanderen"),
    "BeWv" -> ("België","West-Vlaanderen"),
    "Dr" -> ("Nederland", "Drenthe"),
    "Dui" -> ("Duitsland", ""),
    "Fl" -> ("Nederland","Flevoland"),
    "Fr" -> ("Nederland","Friesland"),
    "Fra" -> ("Frankrijk",""),
    "FrVl" -> ("Frankrijk","Vlaanderen"),
    "Gl" -> ("Nederland","Gelderland"),
    "Gn" -> ("Nederland","Groningen"),
    "Lb" -> ("Nederland","Limburg"),
    "NB" -> ("Nederland","Noord-Brabant"),
    "NH" -> ("Nederland","Noord-Holland"),
    "Ov" -> ("Nederland","Overijssel"),
    "Ri" -> ("Duitsland","Rijnland"),
    "Ut" -> ("Nederland","Utrecht"),
    "WaBr" -> ("België","Waals Brabant"),
    "Ze" -> ("Nederland","Zeeland"),
    "ZH" -> ("Nederland","Zuid-Holland")
  )

  def makeLocation(a: Array[String]): Location =
    Location(a(0), a(1), a(2), a(3), a(4), a(5), a(6), a(7), a(8), a(9), a(10), a(11), a(12), a(13), a(14), a(15))

  def allKloekeCodes(f: String):Stream[Location] = scala.io.Source.fromFile(f).getLines().toStream.map(l => makeLocation(l.split("\\t")))
}
