package Wp6

import PageStructure._

import utils.PostProcessXML

import scala.xml._


case class MissivenPageStructure(findZone: Elem => Box, page: Elem) {

  val peetjes = (page \\ "p").zipWithIndex

  def basicPageStructuring() = {

    val candidateHeaders = peetjes.filter({ case (p, i) => i <= 2 && uppercaseity(p) >= 0.8 }).map(_._1)

    if (candidateHeaders.nonEmpty) {
      Console.err.println("\n####### HEADER\n" + candidateHeaders.head)
    }

    val candidateKopregels1 = (page \\ "div").take(2).filter(ab => {
      val b = findZone(ab.asInstanceOf[Elem])
      b.uly < 300 && b.lry < 600
    })

    val candidateKopregels2 = (page \\ "p").take(2).filter(p => {
      if ((p \ "@facs").isEmpty) false else {
        val b = findZone(p.asInstanceOf[Elem])
        b.uly < 250
      }
    })

    val candidateKopregels = if (candidateKopregels1.nonEmpty) candidateKopregels1 else candidateKopregels2

    candidateKopregels.foreach(kr => Console.err.println(kr.text + " " + findZone(kr.asInstanceOf[Elem])))

    val p2 = PostProcessXML.updateElement3(page, x => x.label == "p" || x.label == "ab",
      p => if (candidateKopregels.contains(p))
        p.copy(label = "fw", attributes =
          p.attributes.append(new UnprefixedAttribute("type", "head", Null)).append(new UnprefixedAttribute("place", "top", Null))) else p)
    val p3 = PostProcessXML.updateElement(p2, _.label == "p",
      p => if (candidateHeaders.contains(p)) p.copy(label = "head") else p)
    p3
  }

}
