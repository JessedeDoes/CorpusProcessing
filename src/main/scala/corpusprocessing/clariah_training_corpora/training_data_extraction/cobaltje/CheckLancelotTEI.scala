package corpusprocessing.clariah_training_corpora.training_data_extraction.cobaltje

import corpusprocessing.eindhoven.Eindhoven.getId
import utils.zipUtils

import java.io.File
import java.nio.file.Files
import scala.xml._
object CheckLancelotTEI {
  val oldDownloadDir = "/mnt/Projecten/Corpora/TrainingDataForTools/CobaltExport/2025/download_19_februari_2025/"
  val newDownloadDir = "/data/Lancelot/download"
  val dir = "/mnt/Projecten/Corpora/TrainingDataForTools/CobaltExport/2025/download_19_februari_2025/Gys/LancelotExport/"



  def getJoin_n(n: Node): String =  {
    if ((n \ "join").nonEmpty)
      (n \ "join" \  "@n").text;
    else if ((n \ "@corresp").nonEmpty) { // oude gysseling en cobalt codering......
      val join_n = (List(getId(n).get) ++  (n \ "@corresp").text.split("\\s+").map(_.replaceAll("#","")).toList).sorted.mkString("_")
      join_n
    } else "" }

  def word(w: Node)  = if ( (w \ "seg").nonEmpty) (w \ "seg").text.trim else (w.text.trim)

  def words(d: Node) = (d \\ "w").map(word)

  def taggedText(d: Node) = words(d).mkString(" ")

  def uuid(s: String): String = {
    val bytes = s.getBytes("UTF-8")
    java.util.UUID.nameUUIDFromBytes(bytes).toString
  }


  def uuidFromContent(d: Node) = uuid(taggedText(d))

  def fixPoSforPunct(w: Elem): Elem = {
    val txt:String = if ( (w \ "seg").nonEmpty) (w \ "seg").text.trim else (w.text.trim);
    if (txt.matches("(\\p{P})+")) {
      //Console.err.println(s"Missed punctuation in $w")
      val w1 = corpusprocessing.parlamint.setAtt.setAttribute(w,"pos", "PC")
      w1.copy(label="pc")
    } else w
  }

  def fixJoinForW(w: Elem) = {
       if ( (w \ "join").nonEmpty || (w \ "@corresp").isEmpty )
         w else
         {
           val n = getJoin_n(w)
           // println(s"Fixing missing join, $n")
           w.copy(child = w.child ++ <join n={n}/>)
         }
    }

  def fixMissingJoins(d: Elem): Elem  = utils.PostProcessXML.updateElement(d, _.label=="w", fixJoinForW)
  def fixPunct(d: Elem)  = utils.PostProcessXML.updateElement(d, x => Set("w", "pc").contains(x.label), fixPoSforPunct)
  def patchDocument(d: Elem): Elem  = fixPunct(fixMissingJoins(d))


  def lookAtPidsAndFilenames(zipName: String, compare: Seq[(String,String)] = Seq()) = {
    println(s"\n\n##### $zipName #####")
    val m = compare.toMap
    lazy val paths = zipUtils.find(zipName)
    lazy val inputStreams = paths.map(p => (p.toString -> Files.newInputStream(p)))
    val id2name = inputStreams.sortBy(_._1).take(Integer.MAX_VALUE).map({case (n, s) => {
      val doc = XML.load(s)
      val speak = compare.nonEmpty
      //println(doc \\ "teiHeader")
      if (speak) {
        println(doc.copy(child = Seq()))
        println(doc.child.filter(_.isInstanceOf[Elem]).map(_.asInstanceOf[Elem].copy(child = Seq())))
        val idLikeElements = doc.descendant_or_self.map(n => getId(n)).filter(_.nonEmpty).take(4)
        println(s"${zipName.replaceAll(".*/", "")}: $n: $idLikeElements")
      }
      val uuid = uuidFromContent(doc)
      if (compare.nonEmpty) {
        if (m.contains(uuid))
          {
            println(s"Yep!!!? ${m(uuid)} ~ $n?  ")
          }
      }
      uuid -> n
    }})
    id2name.toList
  }

  def main(args: Array[String]): Unit = {
     val newInfo = new File(newDownloadDir).listFiles().filter(_.getName.endsWith(".zip")).map(f => {
       val zipName = f.getAbsolutePath
       val m = lookAtPidsAndFilenames(zipName)
       m
     }).reduce({case (l1,l2) => l1 ++ l2})
    new File(oldDownloadDir).listFiles().filter(_.getName.endsWith(".zip")).map(f => {
      val zipName = f.getAbsolutePath
      val m = lookAtPidsAndFilenames(zipName, newInfo)
      m
    })
  }
}
