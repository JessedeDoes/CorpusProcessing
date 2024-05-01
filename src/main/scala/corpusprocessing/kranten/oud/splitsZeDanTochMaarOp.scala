package corpusprocessing.kranten.oud
import corpusprocessing.wolkencorpus.postProcessBaBTagging

import scala.xml._
import utils.zipUtils

import java.io.File

/*
    PM: zet dit in de export ipv eerst plakken en dan opsplitsen?

    al newNs = scala.xml.NamespaceBinding("image", "http://www.google.com/schemas/sitemap-image/1.1", sitemapXml.scope)
newNs: scala.xml.NamespaceBinding =  xmlns:image="http://www.google.com/schemas/sitemap-image/1.1" xmlns="http://www.sitemaps.org/schemas/sitemap/0.9"
 */

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

  def setTeiScope(x: Elem): Elem = {
    val y = clearScope(x).asInstanceOf[Elem]
    y.copy(scope = teiScope)
  }

}
import TEIScope._
object splitsZeDanTochMaarOp {

  //val outZip = "/tmp/Gesplitst.zip"

  def writeFiles(f: File, stuff: Iterator[(xml.Node, Int)], inDir: String, outDir: String): Unit = {
    val root = zipUtils.getRootPath(outDir + f.getName.replaceAll(".xml", ".zip"))

    stuff.foreach{case (n,i) =>
      val e = n.asInstanceOf[Elem]
      val newNS = scala.xml.NamespaceBinding("tei", "http://www.tei-c.org/ns/1.0", e.scope)
      val e1 = <TEI xmlns="http://www.tei-c.org/ns/1.0"/>
      val e2 = e1.copy(scope = newNS)
      val e3 = e2.copy(child=e.child)
      lazy val e4 = postProcessBaBTagging.fixDocje(e3,false)
      val e5 = setTeiScope(e3)
      val zipEntry = zipUtils.getWriter(root, s"${f.getName.replaceAll(".xml","")}_$i.xml")
      zipEntry.write(e5.toString()) // .replaceAll("<TEI", "<TEI xmlns=\"http://www.tei-c.org/ns/1.0\" "))
      zipEntry.close()
    }
    root.getFileSystem.close()
    Console.err.println(s"Done ${f.getCanonicalPath}")
  }

  def main(args: Array[String]): Unit = {
    val inDir = args.headOption.getOrElse("/mnt/Projecten/Corpora/Historische_Corpora/17e-eeuwseKranten/TaggedTestVersion7/")
    val outDir = if (args.size > 1) args(1) else "/tmp/Gesplitst/"

    val stuffs: Iterator[(File, Iterator[(xml.Node, Int)])] =
      new java.io.File(inDir)
        .listFiles.sorted.iterator.map(x => x -> (XML.loadFile(x) \\ "TEI").iterator.zipWithIndex)

    stuffs.foreach{case (f, it) => writeFiles(f,it,inDir,outDir)}
  }
}
