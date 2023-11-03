package corpusprocessing.edges.openEdges
import corpusprocessing.edges.openEdges.gesleutel.alignment_files
import java.io.PrintWriter
import scala.xml._
import Verse._
import Alignment._
import Settings._
import java.io.File
import utils.ProcessFolder

object dehelezooi {

  lazy val alleZooi =ProcessFolder.filesIn(new File(Settings.baseDir + "/Alignments")).map(_.getCanonicalPath).filter(_.endsWith(".tsv")).filter(x => x.contains("nl-nl") || x.contains("en-nl") || x.contains("en-en"))

  lazy val b = BibleCorpus(baseDir, alleZooi.toSet)
  def main(args: Array[String]) = {
    b.printBooks()
    b.printBookAlignments()
  }
}
object gesleutel extends BibleCorpus(baseDir, Set( staten_darby, staten_canisius, darby_canisius)) {
  def main(args: Array[String]) = {
    printBooks()
    printBookAlignments()
    // a.foreach(println)
  }
}


/*
1Chr.466        1Chr.469
1Chr.467        1Chr.470
1Chr.468        1Chr.471
1Chr.469        1Chr.472
1Chr.470,1Chr.471       1Chr.473,1Chr.474
1Chr.472,1Chr.473       1Chr.475,1Chr.476


1Thess.1.6      Ende ghy zijt onse navolgers geworden, ende des Heeren, het woort aengenomen hebbende in vele verdruckinge, met blijdschap des heyligen Geests:
1Thess.1.7      Alsoo dat ghy voorbeelden geworden zijt allen den geloovigen in Macedonien ende Achajen.
1Thess.1.8      Want van u is het woort des Heeren luytbaer geworden niet alleen in Macedonien ende Achajen, maer oock in alle plaetsen is uw’ geloove, dat ghy op [{Godt}] hebt, uyt gegaen, so dat wy niet van noode en hebben yet [{daer van}] te spreken.


 */
