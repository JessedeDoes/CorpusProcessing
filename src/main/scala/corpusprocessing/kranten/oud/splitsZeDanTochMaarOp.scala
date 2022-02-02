package corpusprocessing.kranten.oud
import scala.xml._
import utils.zipUtils
import java.io.File

/*
    PM: zet dit in de export ipv eerst plakken en dan opsplitsen?
 */

object splitsZeDanTochMaarOp {
  val inDir = "/mnt/Projecten/Corpora/Historische_Corpora/17e-eeuwseKranten/TaggedTestVersion4/"
  val outDir = "/tmp/Gesplitst/"
  val outZip = "/tmp/Gesplitst.zip"

  val stuffs: Iterator[(File, Iterator[(xml.Node, Int)])] =
    new java.io.File(inDir)
      .listFiles.iterator.map(x => x -> (XML.loadFile(x) \\ "TEI").iterator.zipWithIndex)

  def writeFiles(f: File, stuff: Iterator[(xml.Node, Int)]): Unit = {
    val root = zipUtils.getRootPath(outDir + f.getName.replaceAll(".xml", ".zip"))
    stuff.foreach{case (n,i) =>
      val zipEntry = zipUtils.getWriter(root, s"${f.getName.replaceAll(".xml","")}_$i.xml")
      zipEntry.write(n.toString())
      zipEntry.close()
    }
    root.getFileSystem.close()
    Console.err.println(s"Done ${f.getName}")
  }

  def main(args: Array[String]): Unit = {
    stuffs.foreach{case (f, it) => writeFiles(f,it)}
  }
}
