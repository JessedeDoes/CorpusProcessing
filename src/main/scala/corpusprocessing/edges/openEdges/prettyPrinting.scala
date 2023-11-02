package corpusprocessing.edges.openEdges

import java.io.PrintWriter
import scala.xml._

object prettyPrinting {


  def prettyScalaOld(pw: PrintWriter, e: Elem)  = {
    val pretty = new scala.xml.PrettyPrinter(Integer.MAX_VALUE, 4)
    pw.write(pretty.format(e))
    pw.close()
  }

  def prettyScala(pw: PrintWriter, e: Elem): Unit = {
    val pretty = new XMLPrettyPrinter(4, "w", "idno")
    pw.write(pretty.format(e))
    pw.close()
  }

  def prettyPlain(pw: PrintWriter, e: Elem): Unit = {
    pw.write(e.toString)
    pw.close()
  }

  def prettyJava(pw: PrintWriter, e: Elem)  = {
    val doc = toJava(e)

    import javax.xml.transform.dom.DOMSource
    import javax.xml.transform.stream.StreamResult
    import javax.xml.transform.{OutputKeys, TransformerFactory}
    val transformer = TransformerFactory.newInstance.newTransformer
    transformer.setOutputProperty(OutputKeys.INDENT, "yes")
    transformer.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "4")
    //initialize StreamResult with File object to save to file
    val result = new StreamResult(pw)
    val source = new DOMSource(doc)
    transformer.transform(source, result)
    pw.close()
    //val xmlString = result.getWriter.toString
    //System.out.println(xmlString)
  }

  def toJava(e: Elem)  = {
    new ElemExtras(e).toJdkDoc
  }

  def pretty(pw: PrintWriter, e: Elem): Unit = prettyScala(pw, e) // pretty

  val warnProcInstr = false;

  def pretty(f: String, e: Elem):Unit  = {
    val pw = new PrintWriter(f)
    if (warnProcInstr && e.descendant.exists(_.isInstanceOf[ProcInstr])) {
      Console.err.println(e.descendant.filter(_.isInstanceOf[ProcInstr]))
    }
    pretty(pw,e)
    pw.close()
  }

  val example =    <taxonomy xml:id="subcorpus"> <!-- onderscheid het referentiecorpus en het covid-corpus -->
    <desc xml:lang="nl"><term>Subcorpora</term>: reference en COVID-corpus</desc>
    <desc xml:lang="en"><term>Subcorpora</term>: reference corpus and COVID corpus</desc>

    <category xml:id="reference">
      <catDesc xml:lang="nl"><term>Reference</term>: referentiesubcorpus, tot 2019-10-31</catDesc>
      <catDesc xml:lang="en"><term>Reference</term>: reference subcorpus, until 2019-10-31</catDesc>
    </category>

    <category xml:id="covid">
      <catDesc xml:lang="nl"><term>COVID</term>: COVID subcorpus, vanaf 2019-11-01 onwards</catDesc>
      <catDesc xml:lang="en"><term>COVID</term>: COVID subcorpus, from 2019-11-01 onwards</catDesc>
    </category>
  </taxonomy>

  def main(args: Array[String]): Unit = {
     val pwScala = new PrintWriter("/tmp/scalaPretty.xml")
     val pwJava = new PrintWriter("/tmp/javaPretty.xml")
     prettyJava(pwJava, example)
     prettyScala(pwScala, example)
  }
}


object XmlHelpers {
  implicit def nodeExtras(n: Node) = new NodeExtras(n)
  implicit def elemExtras(e: Elem) = new ElemExtras(e)

  val docBuilder =
    javax.xml.parsers.DocumentBuilderFactory.newInstance().newDocumentBuilder()
}

class NodeExtras(n: Node) {
  import XmlHelpers._
  def toJdkNode(doc: org.w3c.dom.Document): org.w3c.dom.Node =
    n match {
      case Elem(prefix, label, attributes, scope, children @ _*) =>
        val e = n.asInstanceOf[Elem]
        val ns = e.namespace
        // XXX: ns


        val r = doc.createElementNS(ns, label)// doc.createElement(label)
        for (a <- attributes) {
          val prf = a.getNamespace(n)
          println(prf)
          r.setAttributeNS(prf, a.key, a.value.text)
        }
        for (c <- children) {
          r.appendChild(c.toJdkNode(doc))
        }
        r
      case Text(text) => doc.createTextNode(text)
      case Comment(comment) => doc.createComment(comment)
      case EntityRef("&amp;") =>doc.createTextNode("&")
      case EntityRef(txt) => doc.createTextNode(txt)
      // not sure
      case a: Atom[_] => doc.createTextNode(a.data.toString)
      // XXX: other types
      //case x => throw new Exception(x.getClass.getName)
    }
}

class ElemExtras(e: Elem) extends NodeExtras(e) {
  override def toJdkNode(doc: org.w3c.dom.Document) =
    super.toJdkNode(doc).asInstanceOf[org.w3c.dom.Element]

  def toJdkDoc = {
    val doc = XmlHelpers.docBuilder.newDocument()
    doc.appendChild(toJdkNode(doc))
    doc
  }
}

