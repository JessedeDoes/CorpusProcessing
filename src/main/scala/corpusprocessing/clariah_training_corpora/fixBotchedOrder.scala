package corpusprocessing.clariah_training_corpora

import scala.xml._
import fixTokenization.getId
import java.io.File
import utils.{PostProcessXML, ProcessFolder}
object fixBotchedOrder {
  val correctOrderDir = "/mnt/Projecten/Corpora/Historische_Corpora/TrainingCorpora/Mapped2TDN/"
  val exportDir = "/home/jesse/Downloads/CobaltServeExportFixed"
  val orderFixedDir = "/home/jesse/Downloads/CobaltServeExportOrderCorrected"

  lazy val reference: Map[String, File] = allFilesin(new File(correctOrderDir)).filter(_.getName.endsWith(".xml")).map(x => getId(XML.loadFile(x)) -> x).toMap

  def allFilesin(file: File): List[File] = {
    println(file)
    List(file) ++  file.listFiles() ++ file.listFiles().filter(_.isDirectory).flatMap(allFilesin)
  }



  def tokensIn(s: Node)  = s.child.filter(x => x.label == "w" || x.label == "pc")

  val logPermutations = false
  def fixOrder(sCorrect: Elem, sBotched: Elem)  = {
     val correctOrder = tokensIn(sCorrect).filter(!_.text.contains("..")).map(getId).zipWithIndex.toMap
     val botchedOrder = tokensIn(sBotched).map(getId).zipWithIndex.toMap

     if (correctOrder.keySet == botchedOrder.keySet) {
       if (logPermutations && correctOrder != botchedOrder) {
         Console.err.println("\nTry to fix order\n- "
           + tokensIn(sBotched).map(_.text.trim).mkString(" "))
         Console.err.println("- " + tokensIn(sCorrect).map(_.text.trim).mkString(" "))
       }
       sBotched.copy(child = tokensIn(sBotched).sortBy(x => correctOrder(getId(x))))
     } else {
       val wBotched = tokensIn(sBotched).map(_.text.trim).mkString(" ")
       val wCorrect = tokensIn(sCorrect).map(_.text.trim).mkString(" ")
       if (wBotched != wCorrect) {}
       Console.err.println("\nCould not fix:")
       Console.err.println(" - " + wBotched)
       Console.err.println(" - " + wCorrect)
       sBotched
     }
   }

  def fixDocje(d: Elem, i: String) : Elem = {
    val refDoc = XML.loadFile(reference(getId(d)))
    val refSentences = (refDoc \\ "s").map(s => getId(s) -> s.asInstanceOf[Elem]).toMap
    PostProcessXML.updateElement(d, _.label == "s", s => fixOrder(refSentences(getId(s)), s))
  }

  def main(args: Array[String]): Unit = {
    ProcessFolder.processFolder(new File(exportDir), new File(orderFixedDir), {case (i,o) =>
      if (i.endsWith(".xml")) {
        val inDoc = XML.load(i)
        val outDoc = fixDocje(inDoc,i)
        val outTSV = o.replaceAll(".xml$", ".tsv")
        XML.save(o, outDoc, "UTF-8")
      }
    })
  }
}
