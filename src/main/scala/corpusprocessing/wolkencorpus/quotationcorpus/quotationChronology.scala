package corpusprocessing.wolkencorpus.quotationcorpus

import scala.xml.{Node, XML}

object quotationChronology {

   def averageDate(d: Node) = {
     ((d \ "@atLeast").text.toInt + (d \ "@atMost").text.toInt) / 2
   }

   def getHalfCentury(cit: Node) = averageDate((cit \ "date").head)

   lazy val years = new java.io.File(quotationCorpus.outDir)
     .listFiles.filter(true ||  _.getName.contains("8"))
     .iterator
     .flatMap(x => (XML.loadFile(x) \\ "date").iterator.map(averageDate))
     .toStream
     .map(x => 50 * (x / 50))
     .groupBy(identity)
     .mapValues(_.size).toList
     .sortBy(_._1)

  lazy val cits = new java.io.File(quotationCorpus.outDir)
    .listFiles
    .iterator
    .flatMap(x => (XML.loadFile(x) \\ "cit").iterator)

  def main(args: Array[String]): Unit = {
    years.foreach(println)
  }
}

/*

https://docs.google.com/document/d/1A7eAwlLgOTQgvLm5NkSG-pjX1fvCYOFDi7abhXEbLOY/edit


(1250,1)
(1300,3)
(1350,4)
(1400,27)
(1450,492)
(1500,29600)
(1550,52280)
(1600,115248)
(1650,80598)
(1700,43244)
(1750,67501)
(1800,86876)
(1850,118846)
(1900,66132)
(1950,13638)
(2000,3)
 */