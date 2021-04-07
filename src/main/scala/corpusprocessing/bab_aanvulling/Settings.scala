package corpusprocessing.bab_aanvulling
import java.io.File

import bab_conversie._
import database._
import scala.xml._

object Settings {

  val base = "/mnt/Projecten/corpora/Historische_Corpora/BrievenAlsBuitAanvulling"
  val babDir = base + "/!!!2017_21 nov_DANS uploaden/"


  val txt_17 = babDir + "DANS_17E-textfiles CORR_DEF"
  val txt_18 = babDir + "DANS_18E-textfiles CORR_DEF"
  val txt_aanpassing = base + "/aanpassingTxt"

  val allTextFiles = List(txt_17, txt_18, txt_aanpassing).flatMap(d => new File(d).listFiles.filter(_.getName.toLowerCase.endsWith("txt")))

  val textFileMap: Map[String, File] = allTextFiles.map(f => f.getName.replaceAll(".\\w+$", "") -> f).toMap

  val img_aanpassing = base + "/aanpassingImg"

  val img_17 = babDir + "17"
  val img_18 = babDir + "18"

  val xml_17 = "/mnt/Projecten/corpora/Historische_Corpora/BrievenAlsBuitAanvulling/XML/17"
  val xml_18 = "/mnt/Projecten/corpora/Historische_Corpora/BrievenAlsBuitAanvulling/XML/18"
  val xml_aanpassing = base + "/XML/Aanpassing"

  val processed_17 = "/mnt/Projecten/corpora/Historische_Corpora/BrievenAlsBuitAanvulling/Processed/17"
  val processed_18 = "/mnt/Projecten/corpora/Historische_Corpora/BrievenAlsBuitAanvulling/Processed/18"
  val processed_aanpassing = "/mnt/Projecten/corpora/Historische_Corpora/BrievenAlsBuitAanvulling/Processed/Aanpassing"

  val imagePatchDir = "/mnt/Projecten/corpora/Historische_Corpora/BrievenAlsBuitAanvulling/imagePatch/"

  lazy val allProcessedFiles: Array[File] = new File(processed_17).listFiles ++ new File(processed_18).listFiles ++ new File(processed_aanpassing).listFiles
  def att(n: String, v: String): NodeSeq => Boolean = x => { val a = "@" + n; (x \ a).text == v }

  def getPid(xml: Elem) = ((xml \\ "interpGrp").filter(att("type", "pid")) \ "interp" \ "@value").text

  lazy val processedFileMap: Map[String, (Elem,String)] = allProcessedFiles.filter(_.getName.endsWith("xml")).map(x => {
    val xml = XML.loadFile(x)
    val pid = getPid(xml)
    // println(pid)
    pid -> (xml, x.getCanonicalPath)
  }).toMap

  lazy val allXMLFiles: List[File] = ((new File(xml_aanpassing).listFiles()) ++
    (new File(xml_17).listFiles()) ++ (new File(Settings.xml_18).listFiles())).filter(!_.getName.startsWith(".")).toList

  val allImageFiles = (new File(Settings.img_17).listFiles()) ++ (new File(Settings.img_18).listFiles()) // ++ new File(Settings.img_aanpassing).listFiles()

  val imageMap = allImageFiles.map(f => f.getParentFile.getName.replaceAll(".*(17|18).*","$1") + "/" + f.getName -> f).toMap

  val correctedImageDirectory = "/mnt/Projecten/corpora/Historische_Corpora/BrievenAlsBuitAanvulling/correctedImages/"

  val xmlAfterImageCorrections = correctedImageDirectory + "XML"

  val abbrCorrectDir = correctedImageDirectory + "XML_abbrfix/"

  val metaDB = new Database(Configuration("gdb", "svowdb20.ivdnt.loc", "brieven_als_buit_aanvulling_v2", "impact", "impact",  "mysql"))

  def outputPathAfterMetadataProcessing(f: File) = {
    f.getCanonicalPath.replaceAll("/XML/", "/Processed/")
  }

  def toXML = {
    //bab_conversie.MainClass.main(Array(txt_aanpassing, xml_aanpassing))
    //bab_conversie.MainClass.main(Array(txt_17, xml_17))
    //bab_conversie.MainClass.main(Array(txt_18, xml_18))
    metadata.main(Array())
    imageCorrections.main(Array())
    // nu eerst process.sh draaien voor de xml bestandjes....
    //fixAbbr.fixAllDocs
  }

  /*
     Om het daarna af te maken:
     - Ga naar /mnt/Projecten/Corpora/Historische_Corpora/BrievenAlsBuitAanvulling/correctedImages
     - Draai "process.sh" (dat zorgt voor de XML)
     - Draai scala checkImages.scala (kopieert gedraaide plaatjes terug naar de image directory)
     - Ga naar de image directory (/mnt/Projecten/Corpora/Historische_Corpora/BrievenAlsBuitAanvulling/!!!2017_21 nov_DANS uploaden) en rsync naar de ato (of ooit een keer productie)
         * rsync -r 17/ root@corpora.ato.ivdnt.org:/vol1/source-images/zeebrieven/Aanvulling/17
         * rsync -r 18/ root@corpora.ato.ivdnt.org:/vol1/source-images/zeebrieven/Aanvulling/18
     - indexeer de XML in /mnt/Projecten/Corpora/Historische_Corpora/BrievenAlsBuitAanvulling/correctedImages/XML
     - kopieer naar corpora.ato en/of svotmc10
   */
  def main(args: Array[String]): Unit = {
   toXML
  }
}

/*
mysql> show tables;
+---------------------------------------+
| Tables_in_brieven_als_buit_aanvulling |
+---------------------------------------+
| brief                                 |
| corpus                                |
| corpus_bij_extern                     |
| correctie                             |
| foto                                  |
| instellingen                          |
| log                                   |
| relatie                               |
+---------------------------------------+
 */