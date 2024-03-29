package corpusprocessing.clariah_training_corpora.impactGT

import scala.xml._
import java.io.{File,PrintWriter}

import sys.process._
import java.net.URL


object fileStats {
  def downloadFile(url: String, filename: String) = {
    new URL(url) #> new File(filename) !!
  }

  def copy(src: File, dest: File): Unit = {
    import java.io.{File, FileInputStream, FileOutputStream}

    new FileOutputStream(dest) getChannel() transferFrom(
      new FileInputStream(src) getChannel, 0, Long.MaxValue)
  }

  implicit def f(x: String): File = new File(x)

  val dddTagged = "/mnt/Projecten/Corpora/Historische_Corpora/ImpactGT/Page/DDDEnhanced/DDDTagged/"
  val dddPDF = "/mnt/Projecten/Corpora/Historische_Corpora/ImpactGT/Page/DDDEnhanced/DDDPDF/"
  val dddTEI = "/mnt/Projecten/Corpora/Historische_Corpora/ImpactGT/Page/DDDEnhanced/DDDTEI/"
  lazy val docjes = new File(dddTagged).listFiles()
  def main(args: Array[String]) = {
    val stats = new PrintWriter("/mnt/Projecten/Corpora/Historische_Corpora/ImpactGT/Page/DDDEnhanced/stats.txt")
    docjes.foreach(f => {
      val d = XML.loadFile(f)
      val fName = f.getName
      val words = (d \\ "w").size
      val NP = (d \\ "w").count(w => (w \ "@pos").text.contains("NOU-P"))
      val foreign = (d \\ "w").count(w => (w \ "@pos").text.matches("RES.*for.*"))
      val date = (d \\ "interpGrp").filter(x => (x \ "@type").text == "dc:date").map(_.text.trim).mkString("")
      val resourceLink =  (d \\ "interpGrp").filter(x => (x \ "@type").text == "dc:identifier").map(_.text.trim).filter(_.contains("resolver")).mkString("")
      val pdfLink = s"$resourceLink:pdf"
      val nPages = (d \\ "div").filter(x => (x \ "@type").text == "page").size
      val pcGTSId = (d \\ "interp").filter(x => (x \ "@type").text == "pcgtsId").map(_.text.trim).mkString("").replaceAll("pc-","")  //  <interp type="pcgtsId">pc-00531226</interp>
      val pageFile = new File(s"/mnt/Projecten/Corpora/Historische_Corpora/ImpactGT/Page/DDD/$pcGTSId.xml")
      val teiFile = new File(dddTEI + fName)
      val subDir = new File(dddPDF + fName.replaceAll(".xml", ""))

      if (pageFile.exists()) {
        subDir.mkdir()
        downloadFile(pdfLink, subDir.getCanonicalPath + "/" + fName.replaceAll("xml", "pdf"))
        // XML.save(subDir.getCanonicalPath + "/" + fName, d);
        if (teiFile.exists())
          copy(teiFile, new File(subDir.getCanonicalPath + "/" + fName))

        if (pageFile.exists())
          copy(pageFile, new File(subDir.getCanonicalPath + "/" + pcGTSId + ".page.xml"));
        else {
          Console.err.println(s"${pageFile.getCanonicalPath} does not exists")
          System.exit(1)
        }
      }

      stats.println(s"$date\t$fName\t$words\t$NP\t$foreign\t$nPages\t$resourceLink")
    })
    stats.close()
  }
}
