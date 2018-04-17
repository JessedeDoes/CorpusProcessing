package posmapping

import scala.xml._

object CRM {

  val mappingFile = "data/cgr.xml"

  lazy val d = XML.load(mappingFile)

  case class Feature(name: String, value: String)
  def map() =
  {

    val platmans = (d \\ "condition").flatMap(
      c =>
      {
        val crmTags = (c \ "@value").text.split(",")
        crmTags.flatMap(tag =>
          {
            val o = c \ "output"
            o.map(x => tag -> Feature((x \ "@name").text, (x \ "@value").text))
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
