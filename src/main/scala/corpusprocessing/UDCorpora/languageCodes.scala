package corpusprocessing.UDCorpora

import scala.xml._

case class WikiLangInfo(r: Seq[String])  {
  lazy val name = r(0)
  lazy val iso2 = r(1)
  lazy val iso3 = r(2)
  override def toString() = s"$name($iso2,$iso3)"
}

object languageCodes {
  lazy val wikitable = XML.load("data/Glottolog/wikipedia_list.html")
  lazy val rows = (wikitable \\ "tr").map(x => (x \ "td").map(_.text.trim)).filter(_.size > 2)
  lazy val fromTwo = rows.map(WikiLangInfo).map(l => l.iso2 -> l).groupBy(_._1).mapValues(x => x.map(_._2))

  def two2three(c: String)  = fromTwo.get(c).map(_(0).iso3).getOrElse(c)

  def main(args: Array[String])  = {
    fromTwo.foreach(println)
  }
}
