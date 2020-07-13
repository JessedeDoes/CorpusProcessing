package corpusprocessing.MNL

import java.io.PrintWriter

import scala.xml.Elem

object DirksMetaculosity extends corpusprocessing.addProcessedMetadataValues {

  val dataDir = "data/MNL/Metadata/"

  val dirk = scala.io.Source.fromFile(s"$dataDir/mnl.sorted.tsv").getLines.map(l => l.split("\\t").toList).toStream

  val dirkHeader = dirk.head.map(_.toLowerCase)

  val dirkjes: Stream[(String, List[String])] = dirk.tail.map(m => m(0) -> m)

  val docs = scala.io.Source.fromFile(s"$dataDir/mnw_documents.tsv").getLines
    .map(l => l.split("\\t").toList)
    .map(l => {
      val file = l(1).replaceAll(".xml","")
      file -> l
    }).toMap

  def matchFilenames() = {
    val x = new PrintWriter("/tmp/matched_dirkFields.tsv")
    x.println((List("id", "title") ++ dirkHeader).mkString("\t"))

    dirkjes.sortBy(_._1).foreach({ case (f, d) =>
      val doc = docs.getOrElse(f, List())
      if (doc.isEmpty) {
        println(f)
      }
      val title = doc(2)
      val id = doc.headOption.getOrElse("no_id")
      val l = d.mkString("\t")
      x.println(s"$id\t$title\t$l")
    })

    x.close()
  }

  val dirkMatched = scala.io.Source.fromFile(s"$dataDir/matched_dirkFields.tsv").getLines.map(l => l.split("\\t").toList).toStream

  val dirkMatchedFieldnames = dirkMatched.head.map(_.toLowerCase)

  val matchedDirkjes: Stream[(String, List[String])] = dirkMatched.tail.map(m => m(0) -> m)

  val dirkMap: Map[String, List[(String, String)]] = matchedDirkjes.map({case (id,l) => id -> dirkMatchedFieldnames.zip(l)}).toMap

  println(dirkMap("INT_1fd90949-ddb5-44e5-99e2-059018fc06ea"))

  override def enrichBibl(b: Elem)  = {


    val iffy = Set("genre", "subgenre", "author")
    val fields = (b \ "interpGrp")
    val keepFields = fields.filter(f  => !iffy.exists((f \ "@type").text.toLowerCase.contains(_)))
    val iffiez = fields.filter(f  => iffy.exists((f \ "@type").text.toLowerCase.contains(_))).toSeq



    val pid = getField(b,"pid").head


    val oldVals = iffiez.map(i => (i \ "@type").text -> (i \ "interp").map(_.text.trim.replaceAll("\\s+", " ")).filter(_.nonEmpty)).filter(_._2.nonEmpty)

    if (dirkMap.contains(pid)) {
      val dirkies = dirkMap(pid).filter(_._2.nonEmpty).groupBy(_._1).mapValues(_.map(_._2))
        .map({ case (n, v) => <interpGrp type={"dirk:" + n}>{v.map(vl => <interp>{vl}</interp>)}</interpGrp>
        })
      val report =
        s"""
           |####$pid
           |  $oldVals
           |  $dirkies
           |""".stripMargin
      println(report)
      b.copy(child = keepFields ++ dirkies)
    } else b
  }

    def main(args: Array[String]): Unit = {
      import utils.ProcessFolder
      ProcessFolder.processFolder(new java.io.File(corpusprocessing.onw.Settings.mnlTEI), new java.io.File(corpusprocessing.onw.Settings.mnlDirkified), fixFile)
    }
}




// TITEL        GENRE   GENRE   GENRE   SUBGENRE        SUBGENRE        SUBGENRE        SUBGENRE        AUTEUR  OPMERKING
// INT_ce69c331-1b00-487c-a4c0-72f0dd91e0d9        circa_instans.xml       Circa instans   genreLevel1: proza; subgenreLevel1: artesliteratuur; subgenreLevel1: geneeskunde; genre: proza; subgenre: artesliteratuur; subgenre: geneeskunde

