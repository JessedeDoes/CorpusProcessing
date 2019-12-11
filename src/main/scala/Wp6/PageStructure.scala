package Wp6
import scala.xml._

case class Box(uly: Int, ulx: Int, lry: Int, lrx: Int)


object PageStructure {
  def zoneX(p: Elem, b: Node) = {
    val facs = (b \ "@facs").text.replaceAll("#","")
    //println(facs)
    val zone = p.descendant.filter(z => z.label=="zone" && getId(z).get == facs).head
    def ai(s: String): Int = (zone \ s).text.toInt
    Box(ai("@uly"), ai("@ulx"), ai("@lry"), ai("@lrx"))
  }

  def zone(page: Elem)(b: Node) = {
    if (b.label == "p" && (b \ "@facs").isEmpty)
      zoneX(page, (b \\ "lb").head)
    else
      zoneX(page, b)
  }

  def getId(n: Node): Option[String] = n.attributes.filter(a => a.prefixedKey.endsWith(":id") ||
    a.key.equals("id")).map(a => a.value.toString).headOption

  def italicity(e: Elem) = {
    val c1 = ((e \\ "hi").filter(x => (x \ "@rend").text.contains("italic")) \\ "w").size
    val c2 = ((e \\ "hi").filter(x => !(x \ "@rend").text.contains("italic")) \\ "w").size
    if (c1 + c2 == 0) 0.0 else c1 / (c1 + c2).asInstanceOf[Double]
  }

  def uppercaseity(e: Node) = {
    val t = e.text
    val u = t.count(_.isUpper)
    val l = t.count(_.isLower)
    if (u + l == 0) 0.0 else (u / (u+l).asInstanceOf[Double])
  }

  def parseRend(r: String): Map[String, String] = {
    //Console.err.println(r)
    r.split("\\s*;\\s*").filter(x => x.trim.nonEmpty && x.contains(":")).map(
      pv => {
        val a = pv.split("\\s*:\\s*")
        a(0) -> a(1)
      }
    ).toMap
  }

  def css(m: Map[String,String]) = m.map({case (k,v) => s"$k: $v"}).mkString("; ")
}
