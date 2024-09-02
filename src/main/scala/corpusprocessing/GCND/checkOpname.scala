package corpusprocessing.GCND

import scala.xml._
object checkOpname {
  val baseDir = "/mnt/Projecten/Hercules/Corpus-ZNL-Dialecten/DownloadFinaalAugustus2024/"
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

  lazy val missing = codes.filter(c => !allMp4Files.contains(c + ".mp4"))

  def main(args: Array[String]) = {
    println(allWavjes)
    println(allMp3Files)

    missing.foreach(m => {
      val wavje = if (allWavjes.contains(m.toLowerCase + ".wav")) allWavjes(m.toLowerCase + ".wav") else "_"
      println(s"$m $wavje")
    })
  }

}

// ffmpeg -i input.wav -codec:a libmp3lame -qscale:a 2 output.mp3
