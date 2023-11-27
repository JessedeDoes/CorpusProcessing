package corpusprocessing.gekaapte_brieven

import corpusprocessing.gekaapte_brieven.Article.meta

import scala.xml.Text

case class Participant(typ: String, fields: Map[String, String]) {
  val interpjes = false
  lazy val abbr = if (typ == "afzender") "afz" else "ontv"

  def ->(n: String): String = fields.getOrElse(s"${abbr}_$n", "unknown")

  def ->(n: List[String]): String = {
    if (n.isEmpty) "unknown" else if ((this -> n.head) != "unknown") this -> n.head else this -> n.tail
    fields.getOrElse(s"${abbr}_$n", "unknown")
  }

  lazy val event = if (typ == "afzender") "sending" else "receiving"
  lazy val id = if (typ.toLowerCase.startsWith("afz")) this -> "afz_id" else this -> "ontv_id"
  lazy val personId: Option[Text] = if (id != "unknown")  Some(Text(id)) else None


  lazy val hasLocationInfo = fields.keySet.exists(k => k.matches(".*(land|regio|plaats|schip).*") && fields(k).nonEmpty && fields(k) != "unknown")

  lazy val xml = <person xml:id={personId} role={this -> "rol_xln"} gender={this -> "geslacht_xl"}>
    {if (interpjes) fields.filter(_._2.nonEmpty).toList.map { case (n, v) => meta(n, v) }}<persName type="original">{s"${this -> "naam_lett_xl"}"}</persName>
    <persName type="normalized">{s"${this -> s"naam_norm_xln"}"}</persName>
    <occupation>{this -> List("beroep_gecorrigeerd_xln", "beroep_xln")}</occupation>
    <event type={event}>
      <desc>{event}</desc>
      {if (hasLocationInfo) <place>
        <placeName type="original">
          <country>
            {this -> "land_lett_xl"}
          </country>
          <region>
            {this -> "regio_lett_xl"}
          </region>
          <settlement>
            {this -> "plaats_lett_xl"}
          </settlement>
          <location type="ship">
            <placeName type="original">
              {this -> "schip_lett_xl"}
            </placeName>
          </location>
        </placeName>
        <placeName type="normalized">
          <country>
            {this -> "land_norm_xl"}
          </country>
          <region>
            {this -> "regio_norm_xl"}
          </region>
          <settlement>
            {this -> "plaats_norm_xl"}
          </settlement>
          <location type="ship">
            <placeName type="normalized">
              {this -> "schip_norm_xln"}
            </placeName>
          </location>
        </placeName>
      </place>}
    </event>
  </person>
}
