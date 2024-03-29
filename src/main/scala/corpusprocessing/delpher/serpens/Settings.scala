package corpusprocessing.delpher.serpens

import java.io.FileReader
import scala.collection.JavaConverters._

case class Settings(defaultServer: String="?", batchSize: Int=500, maxDocuments:Int=Int.MaxValue,
                    defaultStartDate: String="01-01-1800", defaultEndDate:String = "31-12-1939")
{

}

object SettingsObject extends Settings()
{
  def fromFile(f: String):Settings =
  {
    val p = new java.util.Properties
    p.load(new FileReader(f))
    val props = p.stringPropertyNames().asScala // p.propertyNames()//.asScala.toSet.map(x => x.toString)
    val map: Map[String,String] =  props.map( n => n -> p.getProperty(n.toString)).toMap

    Settings(
      map.getOrElse("defaultServer", SettingsObject.defaultServer),
      map.get("batchSize").map(_.toInt).getOrElse(SettingsObject.batchSize),
      map.get("maxDocuments").map(_.toInt).getOrElse(SettingsObject.maxDocuments),
      map.getOrElse("defaultStartDate", SettingsObject.defaultStartDate),
      map.getOrElse("defaultEndDate", SettingsObject.defaultEndDate)
    )
  }

  val beesten = List("Adder", "Bever", "Beverrat", "Boommarter", "Bunzing", "Das",
    "Dennensnuitkever", "Fret", "Hermelijn", "Huismuis", "Konijn", "Lynx", "Muskusrat",
    "Otter", "Raaf", "Spreeuw", "Vos", "Wezel", "Wolf")
}