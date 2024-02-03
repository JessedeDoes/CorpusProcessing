package corpusprocessing.papiaments.nllb

import java.io.{File, FileInputStream}
import java.util.zip.GZIPInputStream
import scala.xml.XML

// https://anymalign.limsi.fr/
// https://dl.acm.org/doi/pdf/10.1145/3168054
// https://arxiv.org/pdf/2310.13995.pdf
object test_fastalign {
  val stukje0 = "/mnt/Projecten/Papiaments/Corpusdata/NLLB/stukje.tok.tmx.gz"
  val stukje = "/tmp/stukje.tok.tmx.gz"

  lazy val streampje = new GZIPInputStream(new FileInputStream(stukje))

  def main(args: Array[String]) = {

    val dir = args.headOption.getOrElse("/mnt/other/svprre10_data/tagger/papje/nllb_tokenized/")
    val n = if (args.size >= 2) args(1).toInt else 100
    lazy val docs = new File(dir).listFiles().iterator.map(XML.loadFile).take(n)
    tmxWordAlign(docs).addWordAlignment()
  }
}
