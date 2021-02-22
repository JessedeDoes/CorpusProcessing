package corpusprocessing.gysseling

import java.io.PrintWriter

import scala.xml.{Elem, Text}

object DirksMetaculosityCG extends corpusprocessing.addProcessedMetadataValues {

  val dataDir = "/mnt/Projecten/Corpora/Historische_Corpora/CorpusGysseling/Metadata/"

  val inDir = s"$dataDir/CGII"

  val outDir = s"$dataDir/CGII_fixedMeta"

  val dirkMatched = scala.io.Source.fromFile(s"$dataDir/dirkVelden.tsv").getLines.map(l => l.split("\\t").toList).toStream

  val dirkMatchedFieldnames = dirkMatched.head.map(_.toLowerCase)

  val matchedDirkjes: Stream[(String, List[String])] = dirkMatched.tail.map(m => m(1) -> m)

  val dirkMap: Map[String, List[(String, String)]] = matchedDirkjes.map({case (id,l) => id -> dirkMatchedFieldnames.zip(l)}).toMap
    .mapValues(_.map({case (n,v) => if (n.contains("enre") || n.contains("ict")) (n, v.toLowerCase) else (n,v) }))

  println(dirkMap("INT_a76426ad-2f61-4f51-9eec-a27fa4f65d5e"))

  override def enrichBibl(b: Elem)  = {

    // println(b)

    val iffy = Set("genre", "subgenre", "author", "fictionality", "xxtitle")
    val fields = (b \ "interpGrp")

    val keepFields = fields.filter(f  => !iffy.exists((f \ "@type").text.toLowerCase.contains(_)))

    val iffiez = fields.filter(f  => iffy.exists((f \ "@type").text.toLowerCase.contains(_))).toSeq


    val pid = getField(b,"pid").head

    println(pid)

    val oldVals = iffiez.map(i => (i \ "@type").text -> (i \ "interp").map(_.text.trim.replaceAll("\\s+", " ")).filter(_.nonEmpty)).filter(_._2.nonEmpty)

    if (dirkMap.contains(pid)) {
      val dm = dirkMap(pid).toMap
      val level = dm("level")
      val dirkies = dirkMap(pid)
        .filter({case (n,v) => v.replaceAll("[^A-Za-z0-9]", "").nonEmpty})
        .map{case (n,v) => (s"${n}Level$level", v)}
        .filter({case (n,v) => iffy.exists(z => n.contains(z))})
        .groupBy(_._1)
        .mapValues(_.map(_._2))

      val dirkiesXML = dirkies.flatMap({ case (n, v) => <interpGrp type={n}>{v.map(vl => <interp>{vl}</interp>)}</interpGrp> ++ Seq(Text("\n"))})
      val report =
        s"""
           |####$pid
           |  $oldVals
           |  $dirkies
           |""".stripMargin
      println(report)
      b.copy(child = keepFields ++ dirkiesXML)
    } else  {
      Console.err.println(s"Hallo: niets gevonden voor $pid")
      System.exit(1)
      b
    }
  }

    def main(args: Array[String]): Unit = {
      import utils.ProcessFolder
      ProcessFolder.processFolder(new java.io.File(inDir), new java.io.File(outDir), fixFile)
    }
}




// TITEL        GENRE   GENRE   GENRE   SUBGENRE        SUBGENRE        SUBGENRE        SUBGENRE        AUTEUR  OPMERKING
// INT_ce69c331-1b00-487c-a4c0-72f0dd91e0d9        circa_instans.xml       Circa instans   genreLevel1: proza; subgenreLevel1: artesliteratuur; subgenreLevel1: geneeskunde; genre: proza; subgenre: artesliteratuur; subgenre: geneeskunde

