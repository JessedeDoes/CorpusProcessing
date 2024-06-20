package corpusprocessing.corpusutils

import scala.xml._
import java.io.PrintWriter

// LET OP: doet niets met comments en processing instructies
// Verwijdert bovendien namespaces op andere plaatsen dan de root

/*
Ad-hoc workaround (voor edges) om bestanden op te slaan die te groot zijn voor scala XML, dat kennelijk eerst de DOM platslaat tot een String
 */
object SaveHugeXML {
  lazy val testBestandje = XML.load("/mnt/Projecten/Corpora/Historische_Corpora/EDGeS_historical_bible_corpus/TEI/all-word-alignments/PrSol.wordAligned.xml")
   def saveNode(w: PrintWriter, n: Node, root: Boolean = false): Unit = {
     n match {
       case e: Elem =>
         val e1 = e.copy(child=Seq())
         val e2 = if (!root) TEIScope.clearScope(e1) else e1
         val emptyTag = e2.toString().replaceAll("/?>.*", "/>")
         if (e.child.isEmpty)
           w.print(emptyTag)
         else {
           val startTag = emptyTag.replaceAll("/>", ">")
           w.print(startTag)
           e.child.foreach(c => saveNode(w,c))
           val endTag = startTag.replaceAll("[ /].*>", ">").replaceAll("<", "</")
           w.print(endTag)
         }
       case t: Text => w.print(t.text)
       case _ => w.print(n.toString())
     }
   }

  def save(f: String, e: Elem)  = {
    val w = new PrintWriter(f)
    saveNode(w,e , root = true)
    w.close()
  }

  def main(args: Array[String])  = {
     val w = new PrintWriter("/tmp/testSave.xml")
     saveNode(w,testBestandje,root = true)
     w.close()
  }
}
