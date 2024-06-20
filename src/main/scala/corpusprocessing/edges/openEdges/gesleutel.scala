package corpusprocessing.edges.openEdges

import java.io.PrintWriter
import scala.xml._
import Verse._
import Alignment._
import Settings._
import java.io.File
import utils.ProcessFolder


object dehelezooi {
  def main(args: Array[String]) = {
    complete_corpus.printBooks()
    complete_corpus.printBookAlignments()
  }
}

object en_nl {
  def main(args: Array[String]) = {
    subcorpus_en_nl.printBooks()
    subcorpus_en_nl.printBookAlignments()
  }
}

object smallish {
  def main(args: Array[String]) = {
    smaller_sample.printBooks()
    smaller_sample.printBookAlignments()
  }
}

object addWordAlignment {
  def main(args: Array[String])  = {
    complete_corpus.addWordAlignment("Gen", genesis)
    complete_corpus.addWordAlignment("Zeph", zeph)
  }
}

object addWordAlignmentForAll {
  def main(args: Array[String])  = {
    Console.err.println("Aligning from: " + verseAlignedTEIDir)
    new File(verseAlignedTEIDir).listFiles().foreach(f =>
    {
      println(f.getName)
      complete_corpus.addWordAlignment(f.getName.replaceAll(".xml", ""), f.getCanonicalPath)
    })
  }
}

object gesleutel {
  def main(args: Array[String]) = {
    /// smaller_sample.printBookAlignments()
    smaller_sample.addWordAlignment(bookname = "Gen", genesis)
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
