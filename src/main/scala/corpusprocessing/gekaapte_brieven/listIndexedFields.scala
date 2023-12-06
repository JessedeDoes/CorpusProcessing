package corpusprocessing.gekaapte_brieven

import scala.xml._
object listIndexedFields {
  val url = "http://svotmc10.ivdnt.loc:8080/blacklab-server/gekaapte_brieven/"

  lazy val doc = XML.load(url)

  def main(args: Array[String]) = {
    val names = (doc \\ "metadataField").map(x => (x \ "@name").text).sorted
    val lines = names.map(n =>
    s"  ['$n' ${" " * (50 - n.length)},       x        ,      x       ,       x       ,                      ,                     ,        x       ],"


    )
    lines.foreach(println)
  }
}
