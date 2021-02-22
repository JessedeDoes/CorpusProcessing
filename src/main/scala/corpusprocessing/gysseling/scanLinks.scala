package corpusprocessing.gysseling

import utils.PostProcessXML.updateElement4

import scala.xml.{Elem, Text, XML}

object scanLinks {

  case class PageLink(pageNumber: Int, lineNumber: Int, fol: String, scanNumber: Option[Int])

  lazy val fol2scan = (XML.load("../Hilex/data/Gysseling/pagelinks_bloeme.xml") \\ "option").map( o => {
    val scanNumber = (o \ "@value").text.toInt
    val fol = o.text.replaceAll(".*\\] *", "").trim
    fol -> scanNumber
  }).toMap

  lazy val pageLinks: Map[(Int, Int), PageLink] = io.Source.fromFile("../Hilex/data/Gysseling/foliering_bloeme.txt").getLines.map(l => l.trim.split("\\s+")).map(
    l => PageLink(l(0).toInt, l(1).toInt, l(2), fol2scan.get(l(2)))
  ).map(p => (p.pageNumber, p.lineNumber) -> p).toMap


  def addFolio(lb: Elem) =
  {
    val pl = (lb \ "@n").text.split(":")
    val (page,line) = (pl(0).toInt, pl(1).toInt)
    val fol = pageLinks.get( (page, line))

    if (fol.isDefined)
      {
        val edRef: Option[Text] = fol.flatMap(f => Some(Text(f.scanNumber.get.toString)))

        Seq(<pb unit="fol" n={fol.get.fol} ed="http://s2w.hbz-nrw.de/llb/content/zoom/" edRef={edRef}/>, lb)
      }
    else lb
  }

  def addScanLinks(d: Elem) =
  {
    if ((d \\ "interpGrp").filter(g => (g \ "@type").text == "sourceID").text.trim == "corpusgysseling.3000")
      updateElement4(d, _.label == "lb", addFolio)
    else d
  }
}
