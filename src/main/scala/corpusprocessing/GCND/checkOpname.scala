package corpusprocessing.GCND

import scala.xml._
object checkOpname {
  val dir = "/mnt/Projecten/Hercules/Corpus-ZNL-Dialecten/DownloadFinaalAugustus2024/opnames"

  lazy val allMp3Files = new java.io.File(dir).listFiles().map(_.getName).toSet.filter(_.endsWith(".mp3"))

  lazy val allWavjes = io.Source.fromFile("/mnt/Projecten/Hercules/Corpus-ZNL-Dialecten/DownloadFinaalAugustus2024/wav.lst").getLines().map(w => w.replaceAll(".*/", "").toLowerCase -> w).toMap

  lazy val foliaDir = "/home/jesse/workspace/XmlToRdf/data/GCND/Folia"
  lazy val foliaFiles = new java.io.File(foliaDir).listFiles().iterator.map(XML.loadFile)
  lazy val codes  = foliaFiles.map(f => (f \\ "opname_code").text).toSet

  lazy val missing = codes.filter(c => !allMp3Files.contains(c + ".mp3"))

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
