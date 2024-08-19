package corpusprocessing.VandeVoorde
import scala.xml._
import java.io.{File, PrintWriter}
import utils.{HTML, PostProcessXML}
import corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.grouping.groupWithFirst
import corpusprocessing.edges.openEdges.prettyPrinting
import corpusprocessing.kranten.oud.TEIScope._

object CorpusVandeVoorde {
   val baseDir = "/mnt/Projecten/Corpora/Historische_Corpora/VandeVoorde_HistoricalCorpusDutch/"

   val location = s"$baseDir/Corpus - huidige versie/"
   val xmlLoc = s"$baseDir/XML/"
   val metadataFile = s"$baseDir/metadata.tsv"

  lazy val sheet_metadata = {
    val lines = io.Source.fromFile(metadataFile).getLines.map(_.split("\\t").toList).toList
    val header = lines.head.map(_.toLowerCase())
    val entries = lines.tail.map(l => l.indices.map(i => header(i) -> l(i)).toMap)
    entries.map(m => m("file") -> m)
  }.toMap

  lazy val files = new File(location).listFiles().filter(_.getName.endsWith(".txt")).iterator

  lazy val docs: Iterator[(String, Elem)] = files.map(f => {
     val content = io.Source.fromFile(f).getLines().mkString("\n")
     val content1 = content.replaceAll("</header>","</header>\n<text>\n<body>\n<div>").replaceAll("\\s*\n\\s*\n\\s*", "\n<p/>")
     val pseudoc = s"<TEI>${content1}</div></body></text></TEI>"
     val soupdoc = HTML.parse(pseudoc)
     f.getName -> soupdoc.asInstanceOf[Elem]
   })

  // ID	FILE	DOCUMENT	ARCHIVE	GENRE	YEAR	YEAR_detail	PERIOD	PLACE	PLACE_detail	REGION	TRANSCRIBER	NOTES	WOCO

  /*
  <header>
  DOCUMENT:
  ARCHIVE:
  GENRE:
  DATE:
  PLACE:
  TRANSCRIBER:
  NOTES:
  WORD COUNT:
  </header>
   */


 // periode moet als variabele worden meegenomen
  val mapping_header = Map(
    "name" -> "authorLevel1",
    "year_detail" -> "witnessYearLevel1_from",
    "genre" -> "genre",
    "place" -> "place",
    "document" -> "titleLevel1",
    "id" -> "pid",
    "archive" -> "archive",
    "period" -> "period",
    "region" -> "region")

  val mapping_sheet = Map("genre" -> "genre", "place" -> "place", "region" -> "region")

  def uuid(s: String): String = {
    val bytes = s.getBytes("UTF-8")
    java.util.UUID.nameUUIDFromBytes(bytes).toString
  }

  def parseHeader(f: String)(h: Elem): Elem = {
    val header_fields: Map[String, String] = h.text.split("\n").filter(_.contains(":")).map(l => l.split("\\s*:\\s*",-1)).map(a => {
      val name = a(0).trim.toLowerCase()
      val value = a(1).trim.toLowerCase()
      name -> value
    }).toMap ++ Map("pid" -> uuid(h.text))

    val sheet_fields: Map[String, String] = sheet_metadata.getOrElse(f, {
      Console.err.println(s"No sheet metadata for $f")
      Map()
    })

    val all_fieldnames = header_fields.keySet ++ sheet_fields.keySet

    Console.err.println(sheet_fields)

    def getFieldValue(f: String)  = sheet_fields.getOrElse(f, header_fields.getOrElse(f, "undefined"))

    def interp(n: String, v: String)  =  { <interpGrp type={n.replaceAll("\\s+","_")}><interp>{v}</interp></interpGrp> }

    def mappedProperty(n: String): Seq[Node] =  {
      val v = getFieldValue(n)
      val k = n.replaceAll("\\s+","_")
      if (k == "year_detail") {
       val parts = v.trim.split("-")
       val from = parts.head
       val to =  parts.tail.headOption.getOrElse(from)
        Seq(interp("witnessYearLevel1_from",from), interp("witnessYearLevel1_to",to))
      } else {
        val k1 = mapping_header.get(k).map(x => <interpGrp type={x}>
          <interp>
            {v}
          </interp>
        </interpGrp>).getOrElse(Seq[Node]())
        k1
      }
    }

    <teiHeader>
      <fileDesc>
        <titleStmt>
          <title>{getFieldValue("document").replaceAll("_", " ")}  ({getFieldValue("genre")}), {getFieldValue("place")}, {getFieldValue("date")}</title>
          <respStmt>
            <resp>Transcription, encoding</resp>
            <name>Iris Van de Voorde</name>
          </respStmt>
        <respStmt>
          <resp>Conversion, linguistic annotation</resp>
          <name>Instituut voor de Nederlandse Taal</name>
        </respStmt>
      </titleStmt>
        <publicationStmt>
          <publisher>Universiteit Leiden</publisher>
          <pubPlace>Leiden</pubPlace>
          <date>2024</date>
          <availability>
            <p>Data at taalmaterialen.ivdnt.org</p>
          </availability>
          <availability>
            <p>In online corpus retrieval application: https://corpusvandevoorde.ivdnt.org</p>
          </availability>
        </publicationStmt>
        <sourceDesc>
        <listBibl type="originalMetadata">
          <bibl>
            {all_fieldnames.map({n =>  interp(n, getFieldValue(n))})}
          </bibl>
        </listBibl>
          <listBibl xml:id="inlMetadata">
            <bibl>
              {all_fieldnames.map({ n => mappedProperty(n) })}
            </bibl>
          </listBibl>
      </sourceDesc>
      </fileDesc>
    </teiHeader>
  }

  def createLineBreaks(p: Elem)  = {
    val childrenMapped = p.child.flatMap({
      case t: Text => if (t.text.contains("\n"))
        {
          val t1 = t.text.replaceAll("\n+", "<lb/>$1")
          val x = try { XML.loadString(s"<doc>$t1</doc>") } catch {
            case ex: Exception =>  Seq(t)
            case e: Elem => e.child
            case y:Node => Seq(y)
          }
          x
        }
      else Seq(t)
      case x => Seq(x)
    })
    val p1 = p.copy(child=childrenMapped)
    Console.err.println(s"New p: $p1")
    p1
  }

  def markParagraphs(div: Elem) = {
    val groupedChildren = groupWithFirst[Node](div.child, _.label=="p")
    val paragraphs: Seq[Node] = groupedChildren.flatMap(g => {
      if (g.flatMap(_.text).mkString("").trim.nonEmpty) {
        Seq(<p>{g.filter(_.label != "p")}</p>)
      } else g.filter(_.label != "p")
    })
    div.copy(child = paragraphs)
  }

  implicit def t(x: String): Node => Boolean = _.label == x
  implicit def f(x: String): Elem => Elem = _.copy(label=x)
  implicit def c(x: Elem): Elem => Elem = e => x.copy(child=e.child)

  def transformations(f: String): List[(Node => Boolean,  Elem => Elem)] = List(
    t("div") -> markParagraphs,

    t("header") -> parseHeader(f),
    t("ins") -> <add/>,
    t("cancel") -> <gap reason="cancellation"/>,
    t("illeg") -> (i => {
      val com = (i \ "@com").text
      if (com.nonEmpty) {
        <gap reason="illegible"><desc>{com}</desc></gap>
      } else <gap reason="illegible"/>.copy(child=i.child)
    }),
    t("reg") -> (i => {
      val orig = (i \ "@orig").text
        <choice><orig>{orig}</orig> <reg>{i.child}</reg></choice>
    }),
    t("ed") -> (i => {
      val com = (i \ "@com").text
      <note resp="#ed">{com}</note>
    }),
    t("language") -> (i => {
      val lang = (i \ "@value").text // TODO moet je nog codes van maken
      <foreign xml:lang={lang}>{i.child}</foreign>
    }),
    t("illeg") -> <gap reason="illegible"/>,
    t("leftmargin") -> <note place="margin"/>,
    t("rightmargin") -> <note place="margin"/>,
    t("name") -> <name type="person"/>,
    t("pl") -> <name type="place"/>,
    t("ambig") -> <unclear/>,

  )

  def processTextNode(t: Text, inP: Boolean = false): NodeSeq = {
    val x = t.toString.replaceAll("\\[([a-zA-Z0-9]+)\\]", "<expan>$1</expan>")
    val x1 = if (inP) x.replaceAll("\n", " <lb/>\n") else x
    val child = try {
      val z: Seq[Node] = XML.loadString("<x>"  + x1  + "</x>").child
      z
    } catch {
      case e =>
        println(s"\n\n|$x|\nCould not be parsed! Jammer hoor!")
        // e.printStackTrace()
        t.asInstanceOf[NodeSeq]
    }
    child
  }

  def processTextIn(n: Node, inP: Boolean = false): Node = {
    val child: Seq[Node] = n.child.flatMap(
      { case p: Elem if p.label == "p" => processTextIn(p,true)
        case e: Elem => processTextIn(e,inP)
       case t: Text => processTextNode(t,inP)
      case x => x})
    n match  {
      case e: Elem => e.copy(child = child)
      case x => x
    }
  }

  def transForm(f: String, d: Elem)  = {
    val z1 = transformations(f).foldLeft(d)({case (b, (t1,t2)) => PostProcessXML.updateElement(b,t1, t2)})
    setTeiScope(processTextIn(z1).asInstanceOf[Elem])
  }
  def main(args: Array[String])  = {
    val d = new File(xmlLoc)
    d.mkdir()
    d.listFiles().foreach(f => f.delete)

    docs.foreach({ case (f, d) =>
      Console.err.println(f)
      val t: Elem = transForm(f,d)
      val pw = new PrintWriter(xmlLoc + f.replaceAll("txt$", "xml"))
      prettyPrinting.prettyScala(pw, t)
      pw.close()
    })
  }
}


/*
val dinges = <bibl>
  <interpGrp type="genre">
    <interp>administrative</interp>
  </interpGrp>
  <interpGrp type="document">
    <interp>ga londerzeel_schepenbank steenhuffel_1735</interp>
  </interpGrp>
  <interpGrp type="word_count">
    <interp>7200</interp>
  </interpGrp>
  <interpGrp type="transcriber">
    <interp>iv</interp>
  </interpGrp>
  <interpGrp type="date">
    <interp>1735</interp>
  </interpGrp>
  <interpGrp type="place">
    <interp>steenhuffel</interp>
  </interpGrp>
  <interpGrp type="notes">
    <interp>transcriptie (ongepubliceerd) door louis de bondt die door iris van de voorde is aangepast aan de transcriptierichtlijnen die voor die project gehanteerd worden. extra tekst in de marge (soms toegevoegd op een later tijdstip) werd niet opgenomen.</interp>
  </interpGrp>
  <interpGrp type="archive">
    <interp>gemeentearchief londerzeel</interp>
  </interpGrp>
</bibl>
 */
