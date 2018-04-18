package posmapping

/**
  * Dit gaat over de mapping file van CRM naar CGN die uit de MTAS repository komt:
  *
  * https://raw.githubusercontent.com/meertensinstituut/mtas/master/conf/parser/mtas/crm.xml
  */

import scala.xml._

object CRM {

  val mappingFile = "data/CG/crm.xml"

  lazy val d = XML.load(mappingFile)

  case class Feature(name: String, value: String)
  {
    override def toString = s"$name=$value"
  }

  def map() =
  {

    val platmans = (d \\ "condition").flatMap(
      c =>
      {
        val crmTags = (c \ "@value").text.split(",")
        crmTags.flatMap(tag =>
          {
            val o = c \ "output"

            o.map(x => tag -> {
              val v = (x \ "@value").text
              Feature((x \ "@name").text, if (v.nonEmpty) v else "true") }
            )
          }
        )
      }
    )

    val g = platmans.groupBy(_._1).mapValues(_.map(_._2).toSet)
    g.keySet.toList.sorted.foreach(
      k => println(s"$k ${g(k).mkString(",")}")
    )
  }

  def main(args: Array[String]): Unit = {
    map()
  }
}
