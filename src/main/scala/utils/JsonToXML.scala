package utils

import java.io.PrintWriter

import org.json4s
import propositional.Proposition
import utils.PostProcessXML._

import scala.util.{Try,Success,Failure}
import scala.xml._
import org.json4s.JsonDSL._
import org.json4s.Xml.{toJson, toXml}
import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.native.Serialization.writePretty
import posmapping.CHNStyleTags.parser
import posmapping.TagSet.pretty
import sparql2xquery.ValueRestrictionSet

import scala.collection.immutable
//import scala.Predef.Map

object JsonToXML {

  def fromJSON(json: String) = {
    val xml = <data>{toXml(parse(json))}</data>
    xml
  }

  def printXML(xml: Elem, pw: PrintWriter): Unit = {
    pw.println(pretty.format(xml))
    pw.close()
  }

  def munch(f: String): String = {
    io.Source.fromFile(f).getLines.mkString("\n")
  }

  def rearrange(dict: Elem) =  {
    val d0 = dict.copy(child = dict.child.map{case e: Elem => <entry key={e.label}>{e.child}</entry>})
    val d1 = updateElement(d0, _.label=="defns", parseDef)
    d1
  }

  def parseDef(d: Elem) = {
    val t = d.text
    val newText = Try(XML.loadString("<def>" + t  + "</def>")) match {
      case Success(x) => x
      case _ => <def parsed="false">{t}</def>
    }
    val beetjes = (newText \\ "B").filter(b => !(b.text.matches("^[IVX]+([. ]|$).*"))).drop(1)
    if (beetjes.nonEmpty) println(beetjes)
    val markExtraHW = PostProcessXML.updateElement(newText, beetjes.contains(_), e => e.copy(label="HW"))
    newText
  }


  def main(args: Array[String]): Unit = {
    val xml = rearrange(fromJSON(munch(args(0))))
    this.printXML(xml, new PrintWriter(args(1)))
  }
}
