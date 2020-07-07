package corpusprocessing.gysseling
import scala.xml._
object ListOfDocuments {
 val dir = corpusprocessing.onw.Settings.gysselingProcessedMetadataDir

  def main(args: Array[String]): Unit = {
    val d = new java.io.File(dir)
    val apm = corpusprocessing.addProcessedMetadataValues()
    d.listFiles().filter(x => !x.getName.startsWith("0") && !x.getName.startsWith("1")).foreach(f
    => {
      //sprintln(f.getName)

      val d = XML.loadFile(f)
      val listBibl =  apm.findListBibl(d)
      val bibl = (listBibl \\ "bibl").head.asInstanceOf[Elem]
      val ti = (d \\ "title").text
      val id = apm.getField(bibl, "pid").head
      val fieldsForDir = apm.getFields(bibl,  n => Set("genre", "fict")
        .exists( x => n.toLowerCase.contains(x)))
      println(s"$id\t${f.getName}\t$ti\t$fieldsForDir")
    })
  }
}
