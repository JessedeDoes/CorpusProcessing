package zeeuws

import scala.xml._
import utils.PostProcessXML._
object zeeuws_xml_to_db {

   val filename = "/mnt/Projecten/Hercules/DSDD/WZD/selectie.xml"
   lazy val doc = XML.load(filename)

   def markRegions(s: Elem): Elem = {
     val children = s.child.zipWithIndex

     val nc = children.map({case (c,i) =>
       c match {
         case e: Elem if e.label == "placeName" =>
           val after = children.drop(i+1).map(_._1).text.trim
           //println("####\n" + e)
           //println(after)
           if (after.startsWith("(")) {
             e.copy(label="lamePlaceName")
           } else
             e
         case _ => c
       }
     })
     s.copy(child = nc)
   }

  lazy val postprocessedDoc = {
    updateElement(doc, _.label=="usg", markRegions)
  }
  def main(args: Array[String]) = {
    println((postprocessedDoc \\ "lamePlaceName").map(_.text).groupBy(x=>x).mapValues(_.size))
  }
}
