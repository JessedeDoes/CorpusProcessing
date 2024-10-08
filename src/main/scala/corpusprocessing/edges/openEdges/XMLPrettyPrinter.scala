package corpusprocessing.edges.openEdges

/*
 * Copyright Juan Miguel Cejuela (@juanmirocks)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import java.io._
import scala.xml.dtd.DocType
import scala.xml._

/**
 * Utility to pretty-print Standard Scala XML.
 *
 * Specially useful to have beautifully-formatted HTMLs or XHTMLs. You can pretty-format a scala (XML) Node to String or
 * directly write it to a java File, OutputStream, or Writer, for maximum efficiency.
 *
 *
 * More info: https://github.com/juanmirocks/Scala-XML-Pretty-Printer
 *
 * @author Juan Miguel Cejuela (@juanmirocks)
 * @version 2.2.0
 *
 * @param indent: indentation space for a node's subnodes
 * @param pre: elements to be considered as pre-formatted
 */

class XMLPrettyPrinter(indent: Int, pre: String*) {
  require(indent >= 0, "the indent must be greater than or equal to zero")
  require(pre.forall(p => !(p == null || p.isEmpty())), "all pre elements must be non-empty")

  private val preSet = pre.toSet

  //Console.err.println("Pre set:"  + preSet)

  /**
   * Pretty-format the node as a String.
   */
  def format(node: Node): String = {
    val out = stringWriter
    print(node)(out)
    out.toString
  }

  /**
   * Pretty-write the node to given Writer.
   *
   * The writer is closed at the end.
   *
   * @param out Writer to write to
   *
   * @param node to write
   * @param docType (optional, defaults to null) DocType to include (like <!DOCTYPE ...)
   * @param includeXmlDeclaration true/false (optional, defaults to true). If true, the added declaration is: <?xml version="1.0" encoding="UTF-8"?>
   *
   */

  def write(docType: DocType, addXmlDeclaration: Boolean)(out: Writer)(node: Node): Unit =
    withCloseable(out) { out =>
      if (addXmlDeclaration) {
        out write s"""<?xml version="1.0" encoding="${scala.io.Codec.UTF8}"?>"""
        out write ↵
      }
      if (null != docType) {
        out write docType.toString
        out write ↵
      }

      print(node)(out)
    }

  def writeToFile(out: File, docType: DocType = null, addXmlDeclaration: Boolean = true): Node => Unit =
    write(docType, addXmlDeclaration)(fileWriter(out)) _

  def writeToOutputStream(out: OutputStream, docType: DocType = null, addXmlDeclaration: Boolean = true): Node => Unit =
    write(docType, addXmlDeclaration)(outputStreamWriter(out)) _

  //---------------------------------------------------------------------------

  private val < = '<'
  private val > = '>'
  private val </ = "</"
  private val /> = "/>"
  private val ↵ = System.getProperty("line.separator");

  /**
   * Returns the appended text of the node's leaves (children) iff all children are text nodes
   * (or text-like nodes such as Atom[String]). Otherwise return None.
   */
  private def leavesText(node: Node): Option[String] = {
    val sb = new StringBuilder
    def $(children: Seq[Node]): Option[String] =
      if (children.isEmpty) Some(sb.toString)
      else {
        children.head match {
          case s: Text =>
            sb append s.toString; $(children.tail)
          case a: Atom[_] if a.data.isInstanceOf[String] =>
            sb append a.toString; $(children.tail)
          case _ => None
        }
      }
    $(node.child)
  }

  private def mixedContent(n: Node) =  {
    val mc = n.child.exists(x => x.isInstanceOf[Text] && x.text.trim.nonEmpty) && (n.child.exists(_.isInstanceOf[Elem] || n.child.exists(_.isInstanceOf[ProcInstr])))
    mc
  }

  private def print(node: Node, pscope: NamespaceBinding = null, curIndent: Int = 0, inPre: Boolean = false)(
    implicit out: Writer): Unit = {
    def whitespaceTrim(x: String) = x.trim
    val preformatted = inPre || node.isInstanceOf[Group] || preSet.contains(node.label) //note, group.label fails
    def ::(x: String): Unit = out write x
    def :::(x: Char): Unit = out write x
    def __ : Unit = (0 until curIndent).foreach(_ => :::(' '))
    def printNodes(nodes: Seq[Node], newScope: NamespaceBinding, newIndent: Int): Unit =
      nodes.foreach(n => print(n, newScope, newIndent, preformatted))

    node match {
      case sn: SpecialNode =>
        // System.err.println("So special... " + sn)
        if (preSet.contains(node.label)) { __; ::(node.toString); __ } else
        if (preformatted || node.isInstanceOf[scala.xml.EntityRef])  ::(node.toString);
        else ((x: String) => if (!x.isEmpty()) { __; ::(x); ::(↵) })((whitespaceTrim(node.toString)))
      case Group(group) => printNodes(group, pscope, curIndent)
      case e: Elem if (mixedContent(e) || e.label=="desc") => { __; val z= node.toString().trim;  ::(z.replaceAll(""" xmlns="[^"]*"""","")); __; ::(↵) }
      case _ =>
        if (!inPre) __

        if (node.child.isEmpty) {
          ::(leafTag(node))
        }
        else {
          ::(startTag(node, pscope))
          if (preformatted) {
            printNodes(node.child, node.scope, curIndent + indent)
          }
          else {
            val leavesTxt = leavesText(node).map(s => whitespaceTrim(s))
            if (leavesTxt.isDefined) {
              ::(leavesTxt.get)
            }
            else {
              ::(↵)
              printNodes(node.child, node.scope, curIndent + indent)
              __
            }
          }
          ::(endTag(node))
        }

        if (!inPre) ::(↵)
    }
  }

  //---------------------------------------------------------------------------

  /*
   * The following functions were copied outright from [[scala.xml.{Utility, PrettyPrinter}]]
   */

  private def sbToString(f: (StringBuilder) => Unit): String = {
    val sb = new StringBuilder
    f(sb)
    sb.toString
  }

  private def leafTag(n: Node): String = {
    def mkLeaf(sb: StringBuilder): Unit = {
      sb append <
      n nameToString sb
      n.attributes buildString sb
      sb append />
    }
    sbToString(mkLeaf)
  }

  private def startTag(n: Node, pScope: NamespaceBinding): String = {
    // Console.err.println(pScope)
    //val zcope = if (pScope != null) " " + pScope.toString().replaceAll("xmlns:ns\\S+", "") + " " else ""
    //
    def mkStart(sb: StringBuilder): Unit = {
      sb append <
      n nameToString sb
      n.attributes buildString sb
      n.scope.buildString(sb, pScope)
      sb append >
    }
    sbToString(mkStart)
  }

  private def endTag(n: Node): String = {
    def mkEnd(sb: StringBuilder): Unit = {
      sb append </
      n nameToString sb
      sb append >
    }
    sbToString(mkEnd)
  }

  //---------------------------------------------------------------------------

  /*
   * The following are utility functions.
   */

  /**
   * Work with a closeable object/resource (e.g., an InputStream) in a safe manner,
   * that is, always making sure that the object/resource is finally closed.
   */
  private def withCloseable[In <: { def close(): Unit }, Out](obj: In)(f: In => Out): Out =
    try {
      f(obj)
    } finally {
      obj.close()
    }

  private def outputStreamWriter(outputStream: OutputStream): Writer =
    new BufferedWriter(new OutputStreamWriter(outputStream, scala.io.Codec.UTF8.toString))

  private def fileWriter(file: File): Writer =
    new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file), scala.io.Codec.UTF8.toString))

  private def stringWriter(): Writer =
    new StringBuilderWriter

  /**
   * Copy from [[org.apache.commons.io.output.StringBuilderWriter]]
   * and translated to Scala without the unnecessary constructors.
   */
  private class StringBuilderWriter extends Writer with Serializable {
    val builder = new StringBuilder()

    override def append(value: Char): Writer = {
      builder.append(value);
      this
    }

    override def append(value: CharSequence): Writer = {
      builder.append(value);
      this;
    }

    override def append(value: CharSequence, start: Int, end: Int): Writer = {
      builder.appendAll(value.toString().toCharArray(), start, end);
      this;
    }

    def close(): Unit = {}

    def flush(): Unit = {}

    override def write(value: String): Unit =
      if (value != null) {
        builder.append(value);
      }

    def write(value: Array[Char], offset: Int, length: Int): Unit =
      if (value != null) {
        builder.appendAll(value, offset, length);
      }

    def getBuilder: StringBuilder = builder

    override def toString() = builder.toString()
  }

}
