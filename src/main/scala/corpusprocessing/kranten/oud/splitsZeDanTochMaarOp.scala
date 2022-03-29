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
      val e4 = postProcessBaBTagging.fixDocje(e3,false)
      val zipEntry = zipUtils.getWriter(root, s"${f.getName.replaceAll(".xml","")}_$i.xml")
      zipEntry.write(e4.toString().replaceAll("<TEI", "<TEI xmlns=\"http://www.tei-c.org/ns/1.0\" "))
      zipEntry.close()
    }
    root.getFileSystem.close()
    Console.err.println(s"Done ${f.getName}")
  }

  def main(args: Array[String]): Unit = {
    val inDir = args.headOption.getOrElse("/mnt/Projecten/Corpora/Historische_Corpora/17e-eeuwseKranten/TaggedTestVersion5/")
    val outDir = if (args.size > 1) args(1) else "/tmp/Gesplitst/"

    val stuffs: Iterator[(File, Iterator[(xml.Node, Int)])] =
      new java.io.File(inDir)
        .listFiles.iterator.map(x => x -> (XML.loadFile(x) \\ "TEI").iterator.zipWithIndex)

    stuffs.foreach{case (f, it) => writeFiles(f,it,inDir,outDir)}
  }
}
