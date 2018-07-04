package CRM

import Settings._
import Location._



import scala.xml.{Elem, Node, NodeSeq}

import java.text.DateFormat
import java.text.SimpleDateFormat
import java.text.ParseException
import java.util.Date

object Meta
{
  def interp(n:String, v:String):Elem  = <interpGrp type={n}><interp>{v}</interp></interpGrp>

  def optXML(x: Option[Seq[Node]]):NodeSeq = if (x.isEmpty) <none/> else x.get

  val corpusUUID = "791f2c97-5ff3-441e-9e1c-5421f9230b67"

  def locationInfo(c: String) = kloekeByCode.get(c)

  def locationFields(o: Option[Location]):NodeSeq = {
    val z:Option[Seq[Node]] = o.map( l =>
      if (l.asRegion)
        Seq(
          interp("localization_countryLevel1", l.landnaam),
          interp("localization_provinceLevel1", l.provincienaam),
          interp("localization_regionLevel1", "regio-" + l.plaats),
          interp("localization_latLevel1", l.lat),
          interp("localization_longLevel1", l.lng)
        )
      else
        Seq(
          interp("localization_countryLevel1", l.landnaam),
          interp("localization_provinceLevel1", l.provincienaam),
          interp("localization_regionLevel1", l.streek),
          interp("localization_placeLevel1", l.plaats),
          interp("localization_latLevel1", l.lat),
          interp("localization_longLevel1", l.lng)
        ))

    optXML(z)

  }
}

case class Meta(locPlus: String, status: String, kloeke: String, year: String, number: String, id: String)
{
  def idPlus:String = s"$locPlus.$id".replaceAll(s"^${status}_",s"_$status:")

  val numbr = "[0-9]+".r

  lazy val location =  kloekeByCode.get(kloeke) //  kloekeByCode.get(kloeke.replaceAll("a$", "p")))

  lazy val backupLocation = if (location.isDefined) None
  else if (kloekeByCode.contains(kloeke.replaceAll("a$", "p")))
    kloekeByCode.get(kloeke.replaceAll("a$", "p"))
  else
  {
    val cijfers:String = java.lang.String.format("%03d", (numbr.findFirstMatchIn(kloeke).get.group(0).toInt - 500).asInstanceOf[Object])
    val pkloeke = kloeke.replaceAll("[0-9]+", cijfers).replaceAll("r$","p").replaceAll("a$", "p")
    // Console.err.println(s"PROBEER: $pkloeke")
    val x = kloekeByCode.get(pkloeke).map(x => x.copy(asRegion = true))
    // Console.err.println(s"PROBEER: $pkloeke: $x")
    x
  }

  lazy val title:String = {
    if (location.isDefined)
    {
      val l = location.get
      s"${l.provNaamTitel}, ${l.plaats}, $year-$number"
    } else if (backupLocation.isDefined)
    {
      val l = backupLocation.get
      s"${l.provNaamTitel}, regio ${l.plaats}, $year-$number"
    } else
      s"UnknownLocation(${kloeke}), $year-$number"
  }

  // Console.err.println(s"Document: $title")

  // 2018-06-22T08:03

  def datetime():String =
  {
    val now = new Date
    import java.text.SimpleDateFormat
    val yearf = new SimpleDateFormat("yyyy-MM-dd")
    val timef = new SimpleDateFormat("HH:mm")
    val year =  yearf.format(now)
    val time = timef.format(now)

    s"${year}T$time"
  }

  val metaWithNames:Map[String,String] = List(
    ("pid", uuid()),
    ("sourceID", id),
    ("corpusProvenance", "CRM"),
    //("witnessIsOriginalOrNot", status),
    ("localization_kloekecodeLevel1", kloeke),
    ("witnessYearLevel1_from", year),
    ("witnessYearLevel1_to", year),
    ("titleLevel1", title),
    ("ingestTime", datetime())
  ).toMap

  def asXML:NodeSeq = {
    val instance =  <bibl>
      {metaWithNames.map({ case (k, v) => Meta.interp(k, v) })}{Meta.locationFields(if (!location.isDefined) backupLocation else location)}
    </bibl>

    val merged = bibl.mergeBibl(instance)

    <listBibl type="metadata" xml:id="inlMetadata">
      {merged}
    </listBibl>
  }


  def uuid():String =
  {
    val source = Meta.corpusUUID + "." + idPlus
    val bytes = source.getBytes("UTF-8")
    java.util.UUID.nameUUIDFromBytes(bytes).toString
  }
}