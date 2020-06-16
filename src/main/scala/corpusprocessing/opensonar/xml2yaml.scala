package corpusprocessing.opensonar

import scala.xml._

object xml2yaml {
  def toYaml(n: Node):String = {
    val txt = n.child.filter(!_.isInstanceOf[Elem]).text.trim

    val subElements = n.child.filter(_.isInstanceOf[Elem])
    val subsWithoutIndent = subElements.map(toYaml).flatMap(l => l.split("\\n").filter(_.nonEmpty))
    val subs  = subsWithoutIndent.map(l => "  " + l).filter(_.trim.nonEmpty).mkString("\n")
    if (subElements.nonEmpty && txt.nonEmpty)
      s"""
         |
         |-name:$txt
         |${subsWithoutIndent.map(l => " " + l).mkString("\n")}
       """.stripMargin
    else if (subElements.nonEmpty)
    s"""
       |${n.label}:$txt
       |$subs
     """.stripMargin
    else
      s"""
         |${n.label}:$txt
     """.stripMargin
  }

  val example = <root><fields><field>author<group>Author or speaker info</group>
    <harmony>Author or speaker</harmony></field></fields></root>

  def main(args: Array[String]): Unit = {
    println(toYaml(example))
  }
}

object opensonarFields
{
  val xhtml = XML.load("data/OpenSonar/metadatavelden_in_opensonar.xhtml")

  case class Field(name: String, group: String, harmony: String, mapping: String)

  val allFields = ((xhtml \\ "table").take(1) \\ "tr").map(
    tr => {
      val cells = (tr \ "td").map(_.text.trim.replaceAll("\\s+", " "))
      Field(cells(0), cells(3), cells(4), cells(6))
    }
  )

  val description = <metadataFields>
    {
   {allFields.drop(1).map(f =>  <field>{f.name}<group>{f.group}</group><harmonizedName>{f.harmony}</harmonizedName>{f.mapping}<valueMapping></valueMapping></field>
    )}
    }</metadataFields>

  def main(args: Array[String]): Unit = {
    //println(allFields)
    //println(description)
    println(xml2yaml.toYaml(description))
  }
}
