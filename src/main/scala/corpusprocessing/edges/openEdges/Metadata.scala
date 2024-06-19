package corpusprocessing.edges.openEdges

import scala.xml._
import utils.PostProcessXML.updateElement

object Boeken {


  lazy val boekhtml = XML.load("data/Edges/boeken.html")

  lazy val bookId2bookName = (boekhtml \\ "tr").filter(tr => (tr \ "td").size >= 2).map(tr => {
    val cols = (tr \ "td").map(_.text.trim)
    cols(0) -> cols(1)
  }).toMap ++ Map( // dit zijn boeken in de Liesveltbijbel. Ze lijken nergens mee gealigneerd te zijn.
    "3Esd" -> "3 Esdras",
    "4Esd"-> "4 Esdras",
    "PrSal" -> "Prayer of Solomon",
    "Mich" -> "Micah"
  )

  def main(args: Array[String])  = {
    bookId2bookName.foreach(println)
  }
}

object Metadata {
  lazy val xhtml = XML.load("data/Edges/MetadataOpenEDGeS_v101.xhtml")

  lazy val tables = xhtml \\ "table"
  lazy val versions = tables.map(t => {
    val rows = (t \\ "tr")
    val properties = rows.map(r => {
      val cols = (r \\ "td")
      cols(0).text.trim.replaceAll("\\s+", " ") -> cols(1).text.trim.replaceAll("\\s+", " ")
    })
     properties
  })

  def year(bibleId: String) = {
    bibleId.replaceAll(".*([0-9]{4}).*", "$1")
  }

  def languageId(bibleId: String) = {
    val lid = bibleId.replaceAll("_.*", "")
    lid
  }

  val langMap = Map("nl" -> "Dutch", "en" -> "English", "de" -> "German", "sv" -> "Svedish")
  def language(bibleId: String)  = {
    val lid = languageId(bibleId)
    langMap.getOrElse(lid, s"Language_$lid")
  }

  lazy val versionIds = (xhtml \\ "span").filter(s => (s \ "@class").text == "c21").map(_.text.trim).map(_.replaceAll(" .*", ""))

  lazy val metadataForPid: Map[String, Elem] = {
    val stukjes: Map[String, Elem] = versions.zipWithIndex.map({ case (v, i) => {
      val pid = versionIds(i)
      val vPlus = List("bibleId" -> pid, "year" -> year(pid), "language" -> language(pid)) ++ v

      val interpjes =
        vPlus.map { case (k, v) => <interpGrp type={k}>
          <interp>{v}</interp>
        </interpGrp>
        }
      pid -> <bibl>
        {interpjes}
      </bibl>
    }
    }).toMap
    stukjes
  }


  def interp(x:String, y: String): Elem = <interpGrp type={x}><interp>{y}</interp></interpGrp>

  def getMetadata(versionId: String, bookId: String): Elem = {
    val versionMetadata = metadataForPid.getOrElse(versionId, <bibl><note>{s"Missing version $versionId"}</note></bibl>)
    val bookName = Boeken.bookId2bookName.getOrElse(bookId, s"Missing_name_$bookId")
    val pid = s"$versionId.$bookId"
    val extra = List(interp("pid",pid), interp("bookName", bookName), interp("bookId",bookId))
    val bibl = versionMetadata.copy(child= extra++ versionMetadata.child)
    <listBibl type="edgesMetadata">{bibl}</listBibl>
  }

  def getField(meta: Elem, field: String): String = (meta \\ "interpGrp").filter(i => (i \ "@type").text==field).map(x => x \ "interp").map(_.text).mkString("|")

  lazy val xml = {
      <metadata>{metadataForPid.values}</metadata>
    }

  def main(args: Array[String])  = {
    println(versionIds)
    XML.save("data/Edges/metadata.raw.xml", xml)
  }
}

/*
<interp>Missing_name_3Esd</interp>
<interp>Missing_name_4Esd</interp>
<interp>Missing_name_Mich</interp>
<interp>Missing_name_PrSal</interp>
 */