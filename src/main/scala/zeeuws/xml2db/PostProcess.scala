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
            <region use="false" full={(e \ "@full").text}>{e.child}</region>
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
      <region full={Regions(txt)}>{txt}</region>
    } else p
  }

  def postprocess(doc: Elem) = {
    val allEntries = (doc \\ "entry")
    val groupedByVolgnr = allEntries.groupBy(e => (e \ "volgnr").text)
    val gooiMaarWeg = groupedByVolgnr.filter(_._2.size > 1).mapValues(_.drop(1)).values.toSet
    val gooiMaarWegS = gooiMaarWeg.map(_.toString())
    // println(gooiMaarWeg)

    val pruned = updateElement5(doc, e => e.label == "entry" && gooiMaarWegS.contains(e.toString()), e => { Console.err.println("skipping entry!"); Seq() } ).asInstanceOf[Elem]
    // System.exit(1)
    val r = updateElement(pruned, _.label == "placeName", markRegio)
    updateElement(r, _.label == "usg", markUselessRegions)
  }
}