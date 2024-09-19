package corpusprocessing.GCND

import java.io.PrintWriter
import scala.xml._
object checkOpname {
  val baseDir = "/mnt/Projecten/Hercules/Corpus-ZNL-Dialecten/DownloadFinaalAugustus2024/"

  val opnameCodes = io.Source.fromFile("/mnt/Projecten/Hercules/Corpus-ZNL-Dialecten/DownloadFinaalSeptember2024/opname_code.txt").getLines().toSet

  val audioFinaalSeptember: Map[String, Set[String]] = io.Source.fromFile("/mnt/Projecten/Hercules/Corpus-ZNL-Dialecten/DownloadFinaalSeptember2024/audio.lst").getLines.toList.map(l => {
    val code = l.replaceAll(".*/","").replaceAll("\\..*", "")
    code -> l
  }).groupBy(_._1).mapValues(x =>  {
    val s = x.map(_._2).toSet
    if (s.exists(_.toLowerCase.endsWith("mp3"))) s.filter(_.toLowerCase.endsWith("mp3")) else s
  } )

  val dir = s"$baseDir//opnames"
  val oggdir = s"$baseDir/oggfromp3/"
  val mp4dir = s"$baseDir/opnames_mp4/"

  lazy val allMp3Files = new java.io.File(dir).listFiles().map(_.getName).toSet.filter(_.endsWith(".mp3"))
  lazy val allOggFiles = new java.io.File(oggdir).listFiles().map(_.getName).toSet.filter(_.endsWith(".ogg"))
  lazy val allMp4Files = new java.io.File(mp4dir).listFiles().map(_.getName).toSet.filter(_.endsWith(".mp4"))
  lazy val allWavjes = io.Source.fromFile("/mnt/Projecten/Hercules/Corpus-ZNL-Dialecten/DownloadFinaalAugustus2024/wav.lst").getLines().map(w => w.replaceAll(".*/", "").toLowerCase -> w).toMap

  lazy val foliaDir = "/home/jesse/workspace/XmlToRdf/data/GCND/Folia"

  lazy val foliaFiles = new java.io.File(foliaDir).listFiles().iterator.map(XML.loadFile)
  lazy val codes  = foliaFiles.map(f => (f \\ "opname_code").text).toSet

  lazy val missing = opnameCodes.filter(c => !allMp4Files.contains(c + ".mp4"))
  lazy val pw = new PrintWriter("/mnt/Projecten/Hercules/Corpus-ZNL-Dialecten/DownloadFinaalSeptember2024/convert_to_mp4.lst")
  def main(args: Array[String]) = {
    //println(allWavjes)
    // println(allMp3Files)
    println(missing.size)
    missing.foreach(m => {
      println(m + "\t" + audioFinaalSeptember(m).head)
      pw.println(audioFinaalSeptember(m).head)
    })
    pw.close()
  }
}

// ffmpeg -i input.wav -codec:a libmp3lame -qscale:a 2 output.mp3
