package clariah.standoffmarkup

import java.io.{File, PrintWriter}
import java.text.SimpleDateFormat
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import clariah.standoffmarkup.TEI2NAF.tei2naf

import scala.xml.{Elem, Node, Text, XML}

object TEI2NAF {

  private val splittingTags = Set("lb", "p", "div", "head", "fw")

  def toTextStructure(n: NodeWithOffsets): Node = {
    val nodeType = n.node.getClass.getName.replaceAll(".*\\.", "")
    n.node match {
      case t: Text =>
          <textnode offset={n.start.toString} length={n.length.toString}/>
      case e: Elem =>
        val down = n.children.map(toTextStructure)
        val attributes = n.node.attributes.map(a => <attribute name={a.prefixedKey} value={a.value.text}/>)
        <element id={n.id} offset={n.start.toString} length={n.length.toString} name={n.label}>
          {attributes}{down}
        </element>
      case _ =>
          <node type={nodeType} offset={n.start.toString} length={n.length.toString}/>
    }
  }

  def toTextUnits(n: NodeWithOffsets): scala.xml.NodeSeq = {
    val nodeType = n.node.getClass.getName.replaceAll(".*\\.","")
    if (n.node.isInstanceOf[Elem]) {
      val down = n.children.flatMap(toTextUnits)
      val attributes = n.node.attributes.map(a => <attribute name={a.prefixedKey} value={a.value.text}/>)
      <tunit id={n.id} offset={n.start.toString} length={n.length.toString} class={n.label}/> ++ down
    } else {
       Seq()
    }
  }

  def tei2naf(d: Elem) = {
    val textNode = (d \\ "text").headOption
    textNode.map(n => {
      val n1 = StandoffMarkup.createStandoffMarkup(n,0)
      val txt = n1.text

      lazy val pcdata = txt
      lazy val cdata =  scala.xml.Unparsed("<![CDATA[" + txt   + "]]>")
      val allTokens = StandoffTokenizing.tokenize(n1, x => splittingTags.contains(x.label))
        ._1.zipWithIndex.map({case (t,i) =>
          val id = s"w.$i"
          <wf id={id} offset={t.start.toString} length={(t.end-t.start).toString}>{t.word}</wf>
      })
      val timestamp = LocalDateTime.now.format(DateTimeFormatter.ofPattern("YYYYMMdd_HHmmss"))
      <NAF version="v3.1" xml:lang="nl">
      <nafHeader>
        <fileDesc
          title={(d \\ "title").text}
          filename={(d \\ "idno").filter(x => (x \ "@type").text == "pid").headOption.map(_.text).getOrElse("__")}/>
        <linguisticProcessors layer="raw">
          <lp name="tei2nafsimple" version="0.0" timestamp={timestamp}/>
        </linguisticProcessors>
        <linguisticProcessors layer="tunits">
          <lp name="tei2nafsimple" version="0.0" timestamp={timestamp}/>
        </linguisticProcessors>
        <linguisticProcessors layer="text">
          <lp name="tei2nafsimple" version="0.0" timestamp={timestamp}/>
        </linguisticProcessors>
      </nafHeader>
        <raw>{cdata}</raw>
        <text>{allTokens}</text>
        <tunits>{toTextUnits(n1)}</tunits>
      </NAF>
      //<textStructure type="TEI" namespace="http://www.tei-c.org/ns/1.0">{toTextStructure(n1)}</textStructure></NAF>
    })
  }

  def main(args: Array[String]): Unit = {
    utils.ProcessFolder.processFolder(new File(args(0)), new File(args(1)), (in,out) => {
      val inDoc = XML.loadFile(in)
      tei2naf(inDoc).foreach(
        XML.save(out, _, "UTF-8")
      )
    })
  }
}

object MissivenToNAF {
  def main(args: Array[String]): Unit = {
    TEI2NAF.main(Array(DataLocations.TEIFolder, DataLocations.tei2nafFolder))
  }
}
