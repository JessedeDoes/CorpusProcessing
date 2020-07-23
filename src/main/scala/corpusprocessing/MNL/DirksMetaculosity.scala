package corpusprocessing.MNL

import java.io.PrintWriter

import corpusprocessing.gysseling.DirksMetaculosityCG.{dirkMap, getField}
import utils.PostProcessXML

import scala.xml.{Elem, Text, XML}
import scala.util.{Failure, Success, Try}
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

    // println(b)

    val iffy = Set("genre", "subgenre", "author", "fictionality", "xxtitle")
    val fields = (b \ "interpGrp")

    val keepFields = fields.filter(f  => !iffy.exists((f \ "@type").text.toLowerCase.contains(_)))

    val iffiez = fields.filter(f  => iffy.exists((f \ "@type").text.toLowerCase.contains(_))).toSeq


    val pid = getField(b,"pid").head

    println(s"Zoek naar pid=$pid")

    val oldVals = iffiez.map(i => (i \ "@type").text -> (i \ "interp").map(_.text.trim.replaceAll("\\s+", " ")).filter(_.nonEmpty)).filter(_._2.nonEmpty)

    if (dirkMap.contains(pid)) {
      val dm = dirkMap(pid).toMap
      val level = "1" // dm("level")
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
      //System.exit(1)
      b
    }
  }

  def enrichBiblOld(b: Elem)  = {


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
      //println(report)
      b.copy(child = keepFields ++ dirkies)
    } else b
  }


  val fakeLT= "‹"
  val fakeGT = "›"

  import scala.util.matching.Regex._
  val fakeTag = "‹.*?›".r
  def fixOneFake(z: String) = {

    val t = z.replaceAll(fakeLT,"<").replaceAll(fakeGT, ">").replaceAll("'","").replaceAll("&quot;","").replaceAll("([^\\s<>]+)=([^\\s<>]+)", "$1=\"$2\"")
    Console.err.println(s"$z --> $t")
    t
  }

  def fixFake(s: String) = fakeTag.replaceAllIn(s, z => fixOneFake(z.toString()) )

  def fixFakeTags(interp: Elem): Elem = {
    val txt = <hoera>{interp.child}</hoera>.toString()
    val relevant = txt.contains(fakeLT)
    val t = fixFake(txt)

    val z = if (!relevant) interp else Try{interp.copy(child = XML.loadString(t).child)} match {
      case Success(x) => x
      case Failure(x) =>
        Console.err.println(x)
        interp
    }
    //if (relevant)
      //Console.err.println("‹› in" +  z)
    z
  }

  override def fixFile(in: String, out: String) = {
    val d = addProcessedMetadataValues(XML.load(in))
    val d1 = PostProcessXML.updateElement(d, x => Set("interp","note").contains(x.label), fixFakeTags)
    if (this.nice) {
      val p = new scala.xml.PrettyPrinter(300, 4)
      val t = p.format(d1)
      val w = new java.io.PrintWriter(out)
      w.write(t)
      w.close()
    } else XML.save(out, d1, enc = "UTF-8")
  }

  def main(args: Array[String]): Unit = {
    import utils.ProcessFolder
    ProcessFolder.processFolder(new java.io.File(corpusprocessing.onw.Settings.mnlTEI), new java.io.File(corpusprocessing.onw.Settings.mnlDirkified), fixFile)
  }
}




// TITEL        GENRE   GENRE   GENRE   SUBGENRE        SUBGENRE        SUBGENRE        SUBGENRE        AUTEUR  OPMERKING
// INT_ce69c331-1b00-487c-a4c0-72f0dd91e0d9        circa_instans.xml       Circa instans   genreLevel1: proza; subgenreLevel1: artesliteratuur; subgenreLevel1: geneeskunde; genre: proza; subgenre: artesliteratuur; subgenre: geneeskunde

