package corpusprocessing.corpusutils

import scala.xml._
object TEIScope {
  def clearScope(x: Node): Node = x match {
    case e: Elem if e.label == "formula" =>
      val e0 = e.copy(scope = TopScope, child = e.child.map(clearScope))
      val e1 = <math xmlns="http://www.w3.org/1998/Math/MathML" xmlns:m="http://www.w3.org/1998/Math/MathML">
        {e0.child}
      </math>
      e0.copy(child = e1)
    case e: Elem => e.copy(scope = TopScope, child = e.child.map(y => clearScope(y)))
    case o => o
  }

  val teiScope = XML.loadString("""<TEI xmlns:tei="http://www.tei-c.org/ns/1.0" xmlns="http://www.tei-c.org/ns/1.0"  xmlns:xi="http://www.w3.org/2001/XInclude"/>""").scope // NamespaceBinding("tei", "http://www.tei-c.org/ns/1.0", TopScope)

  def setTeiScope(x: Node): Elem = {
    val y = clearScope(x).asInstanceOf[Elem]
    y.copy(scope = teiScope)
  }
}
