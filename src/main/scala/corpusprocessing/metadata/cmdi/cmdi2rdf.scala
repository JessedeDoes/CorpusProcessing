package corpusprocessing.metadata.cmdi
import scala.xml._
import java.io.PrintWriter
object Indent {
  val indentation = "     "
}

object Prefix {
  val prefixDef = Map("test" -> "http://test/")
  lazy val prefix = "test"
  lazy val prefixURL = Prefix.prefixDef(prefix)
  val componentElement = "Component"
  val elementElement = "Element"
}
import Indent._
import Prefix._

case class Component(n: Node) {
  val name = (n \ "@name").text
  val qname = s"$prefix:$name"
  lazy val elements = (n \ elementElement).map(CMDI_Element)
  lazy val subComponents = (n \ componentElement).map(Component)
  lazy val attributes = (n \ "AttributeList" \  "Attribute").map(Attribute)

  def list(indent: Int=0) = {
    println(indentation * indent + "component:" + name)
    println(indentation * (indent +1) +  "elements:")
    elements.foreach(_.list(indent+2))
    println(indentation * (indent + 1) + "subcomponents:")
    subComponents.foreach(c => println(indentation * (indent+2) + c.name))
  }

  def objectProperties = subComponents.map(sc =>
    s""" Declaration(ObjectProperty($prefix:${name}2${sc.name}))
      | ObjectPropertyDomain($prefix:${name}2${sc.name} $qname)
      | ObjectPropertyRange($prefix:${name}2${sc.name} ${sc.qname})
      |""".stripMargin).mkString("\n\n") +
      elements.filter(_.isClass).map(sc => {
        s""" Declaration(ObjectProperty($prefix:${name}2${sc.name}))
           | ObjectPropertyDomain($prefix:${name}2${sc.name} $qname)
           | ObjectPropertyRange($prefix:${name}2${sc.name} ${sc.qname})
           |""".stripMargin
      }).mkString("\n\n")

  def dataProperties = (elements).map(e =>
    s"""Declaration(DataProperty($prefix:${name}_${e.name}))
       | DataPropertyDomain($prefix:${name}_${e.name} $qname)
       |""".stripMargin).mkString("\n\n")

  def classDecl: String = s"Declaration(Class($qname))"

  def owl =
    s"""$classDecl
       |$dataProperties
       |$objectProperties
       |""".stripMargin
}

case class Attribute(n: Node)  {
  val name = (n  \ "Name" ++ n \ "@name").map(_.text).headOption.getOrElse("NOPE")
  val qname = s"$prefix:$name"
  val valueType = (n \ "Type").text
}

case class CMDI_Element(n: Node) {
  val name = (n \ "@name").text
  val qname = s"$prefix:$name"
  val valueScheme = (n \ "@ValueScheme").text

  def list(indent: Int=0) = println(indentation* indent + name + s" [${valueScheme}]")

  lazy val attributes = (n \ "AttributeList" \  "Attribute").map(Attribute)

  def dataProperties = (attributes).map(e =>
    s"""Declaration(DataProperty($prefix:${name}_${e.name}))
       | DataPropertyDomain($prefix:${name}_${e.name} $qname)
       |""".stripMargin).mkString("\n\n")

  def classDecl: String = s"Declaration(Class($qname))"

  def owl =
    s"""$classDecl
       |$dataProperties
       |""".stripMargin

  def isClass = dataProperties.size > 0
}

case class Profile(n: Elem)  {
  val name = (n \ "@name").text
  lazy val components = (n \\ componentElement).map(Component).groupBy(_.name).mapValues(_.head).values.toList
  lazy val elements = (n \\ elementElement).map(CMDI_Element).groupBy(_.name).mapValues(_.head).values.toList

  def list(indent: Int=0) = {
    println(name)
    println("components:")
    components.foreach(_.list(indent+1))
  }

  def owl: String =
    s"""Prefix($prefix:=<$prefixURL>)
       |Ontology(<$prefixURL>
       |${components.map(c => c.owl).mkString("\n\n")}
       |${elements.filter(_.isClass).map(e => e.owl).mkString("\n\n")}
       |)
       |""".stripMargin
}

object cmdi2rdf {
  val lat_corpus = "/home/jesse/Downloads/TextProfileINT.xml"
  lazy val lat_profile =Profile(XML.load(lat_corpus))

  def main(args: Array[String]) = {
    lat_profile.list()
    val x = new PrintWriter("/var/lib/tomcat9/webapps/dictionary_data/int_text_profile.fss")
    x.println(lat_profile.owl)
    x.close()
  }
  // http://localhost:8080/webvowl/#iri=http://localhost:8080/dictionary_data/int_text_profile.fss 
}


/*

Ontology(&lt;<xsl:value-of select="$model/prefix[1]/text()"/>&gt; # Dictionary: <xsl:value-of select="$optDict"/>


    <xsl:for-each select="$model/class"><xsl:variable name="name"><xsl:value-of select="@name"/></xsl:variable><xsl:if test="(not ($keep_only_what_is_found)) or $classes_found//class[./text()=$name]">

       Declaration(Class(<xsl:value-of select="$prefix"/>:<xsl:value-of select="@name"/>))
   <xsl:if test="./@parentClass">SubClassOf(<xsl:value-of select="$prefix"/>:<xsl:value-of select="@name"/><xsl:text> </xsl:text><xsl:value-of select="$prefix"/>:<xsl:value-of select="@parentClass"/>)<xsl:text>&#x0a;</xsl:text></xsl:if>
   <xsl:if test="@unionOf">EquivalentClasses(<xsl:value-of select="$prefix"/>:<xsl:value-of select="@name"/><xsl:text> </xsl:text>ObjectUnionOf(<xsl:for-each select="tokenize(@unionOf, '\s+')"><xsl:value-of select="$prefix"/>:<xsl:value-of select="."/><xsl:if test="position() != last()"><xsl:text> </xsl:text></xsl:if></xsl:for-each>))<xsl:text>&#x0a;</xsl:text></xsl:if></xsl:if>
</xsl:for-each>

    <xsl:for-each select="$model/relation"><xsl:variable name="name"><xsl:value-of select="@name"/></xsl:variable><xsl:if test="(not ($keep_only_what_is_found)) or $relations_found//relation[./text()=$name]">

       Declaration(ObjectProperty(<xsl:value-of select="$prefix"/>:<xsl:value-of select="@name"/>))
   <xsl:text>   </xsl:text>ObjectPropertyDomain(<xsl:value-of select="$prefix"/>:<xsl:value-of select="@name"/><xsl:text> </xsl:text><xsl:value-of select="$prefix"/>:<xsl:value-of select="@domain"/>)
   <xsl:text>   </xsl:text>ObjectPropertyRange(<xsl:value-of select="$prefix"/>:<xsl:value-of select="@name"/><xsl:text> </xsl:text><xsl:value-of select="$prefix"/>:<xsl:value-of select="@range"/>)</xsl:if>
</xsl:for-each>

    <xsl:for-each select="$model/field"><xsl:variable name="name"><xsl:value-of select="@name"/></xsl:variable><xsl:if test="(not ($keep_only_what_is_found)) or $fields_found//field[./text()=$name]">

       Declaration(DataProperty(<xsl:value-of select="$prefix"/>:<xsl:value-of select="@name"/>))
       <xsl:text>   </xsl:text>DataPropertyDomain(<xsl:value-of select="$prefix"/>:<xsl:value-of select="@name"/><xsl:text> </xsl:text><xsl:value-of select="$prefix"/>:<xsl:value-of select="@domain"/>)</xsl:if>

    </xsl:for-each>
    )
 */