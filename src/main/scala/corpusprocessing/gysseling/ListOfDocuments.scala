package corpusprocessing.gysseling
import corpusprocessing.gysseling.ListOfDocuments.dir
import corpusprocessing.metadata.addProcessedMetadataValues

import scala.xml._

object ListOfDocuments {
  val dir = DirksMetaculosityCG.outDir // corpusprocessing.onw.Settings.gysselingProcessedMetadataDir

  def listFilesInDir(dir: String) = {
    val d = new java.io.File(dir)
    val apm = addProcessedMetadataValues()
    d.listFiles().filter(x => !x.getName.startsWith("0") && !x.getName.startsWith("1")).foreach(f
    => {
      //sprintln(f.getName)

      val d = XML.loadFile(f)
      val listBibl =  apm.findListBibl(d)
      val bibl = (listBibl \\ "bibl").head.asInstanceOf[Elem]
      val ti = (d \\ "title").text
      val id = apm.getField(bibl, "pid").head
      val fieldsForDir = apm.getFields(bibl,  n => Set("genre", "fict", "auth")
        .exists( x => n.toLowerCase.contains(x))).groupBy(_._1).mapValues(s => s.map(_._2).mkString("|"))
      println(s"$id\t${f.getName}\t$ti\t$fieldsForDir")
    })
  }

  def main(args: Array[String]): Unit = {
    listFilesInDir(dir)
  }
}


