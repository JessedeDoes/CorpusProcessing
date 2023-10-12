package zeeuws.xml2db

import scala.xml._
import utils.PostProcessXML._

object PostProcess {
  def markUselessRegions(s: Elem): Elem = {
    val children = s.child.zipWithIndex

    val nc = children.map({ case (c, i) =>
      c match {
        case e: Elem if e.label == "region" =>
          val after = children.drop(i + 1).map(_._1).text.trim

          if (after.startsWith("(")) {
            <region use="false" full={(e \ "@full").text}>
              {e.child}
            </region>
          } else
            e
        case _ => c
      }
    })
    s.copy(child = nc)
  }

  def markRegio(p: Elem): Elem = {
    val txt = p.text.toLowerCase().trim;
    if (Regions.contains(txt)) {
      <region full={Regions(txt)}>
        {txt}
      </region>
    } else p
  }

  def postprocess(doc: Elem) = {
    val r = updateElement(doc, _.label == "placeName", markRegio)
    updateElement(r, _.label == "usg", markUselessRegions)
  }
}