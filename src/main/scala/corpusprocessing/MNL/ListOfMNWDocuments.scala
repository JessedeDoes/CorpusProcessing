package corpusprocessing.MNL

import scala.xml._

object ListOfMNWDocuments {

  val dir = "/mnt/Projecten/Corpora/Historische_Corpora/MNL-TEI/Nederlabversie/Metaculous/"

  val titles = new java.io.PrintWriter("/tmp/titles.txt")

  def listFilesInDir(dir: String): Unit = {
    val d = new java.io.File(dir)
    val apm = corpusprocessing.addProcessedMetadataValues()


    d.listFiles().filter(_.isFile).foreach(f
    => {
      //sprintln(f.getName)

      val d = XML.loadFile(f)
      val listBibl =  apm.findListBibl(d)
      val bibl = (listBibl \\ "bibl").head.asInstanceOf[Elem]
      val ti = (d \\ "title").head.text.trim.replaceAll("\\s+", " ")
      val id = apm.getField(bibl, "pid").head
      val fieldsForDir = apm.getFields(bibl,
        n => Set("genre", "fict", "title", "author").exists( x => n.toLowerCase.contains(x)))
        .map({case (n,v) => s"$n: $v"}).mkString("; ")
      titles.println(s"$id\t${f.getName}\t$ti\t$fieldsForDir")
    })

    d.listFiles.filter(_.isDirectory).foreach(f => listFilesInDir(f.getCanonicalPath) )
  }

  def main(args: Array[String]): Unit = {

    listFilesInDir(dir)
    titles.close()
  }
}