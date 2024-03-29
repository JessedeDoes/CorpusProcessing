package serpens
import java.io.PrintWriter
import java.nio.file.Path
import java.nio.file._
import corpusprocessing.delpher.KBZipje

import scala.collection.JavaConversions._
import scala.xml._
import scala.util.matching._
import utils._
import corpusprocessing.delpher._
import scorpusprocessing.delpher.KBKwic

case class ColumnConfiguration(query:String, textColumn:Int, idColumn:Int)

case class TSVWriter(zipFilename: String, columns: Seq[String],
                     snipper: (TextQuery, Node, Node) => List[utils.Concordance] = {case (textQuery, doc, mainMeta)  =>
                            KBKwic.concordance(textQuery, doc, mainMeta)})
{
  lazy val zipje = new KBZipje(zipFilename)
  lazy val textQuery = zipje.textQuery

  // dit werkt dus niet ... Je moet weten of alles phrase is, want dan moeten de non-phrases geen hits geven

  def makeLoadQuery(tableName: String):String =
  {
    s"""|set schema 'artikelen';
       |drop table if exists $tableName;
       |create table $tableName
       |(
       |${columns.map(f => s"${f.replaceAll("\\.", "_")} text ${if (f=="idno") " primary key "  else ""}").mkString(",\n")}
       |); 
       |\\cd 'tables';
       |\\copy $tableName from '$tableName.tsv.txt' with delimiter E'\\t'
       |alter table $tableName add column classification text default '';
       |alter table $tableName add column comment text;
       |ALTER TABLE $tableName ADD CONSTRAINT ${tableName}_conceptfk FOREIGN KEY (classification) REFERENCES artikelen.classification_list (id) MATCH FULL;
       |delete from beesten where beest='$tableName';
       |insert into beesten (beest) VALUES ('$tableName');
     """.stripMargin
  }



  def printCSV(pw: Option[java.io.PrintStream] = None):Unit =
  {

    val pw1 = pw.getOrElse(System.out)
    val lines = zipUtils.findPath(zipje.root, p => p.toString.endsWith(".xml")).par.map(p =>
      writeOneLine(p))
    lines.foreach(l => pw1.println(l))
  }

  def parseBibl(b: Node, prefix: String = ""):Map[String,String] = (b \\ "interpGrp")
    .map(g => prefix + (g \ "@type").text -> (g \ "interp" \ "@value").mkString("|"))
    .toMap




  def writeOneLine(path: Path): String =
  {
    val doc = XML.load(Files.newInputStream(path))

    val mainMeta = (doc \\ "listBibl").filter(b => (b \ "@id").toString == "inlMetadata").head
    val profile = (doc \\ "listBibl").filter(b => (b \ "@id").toString == "profile").head
    val articleMeta = (doc \\ "listBibl").filter(b => (b \ "@id").toString == "articleMetadata").head

    val metaFields = parseBibl(mainMeta)



    val articleMetaFields = parseBibl(articleMeta, "article.") // de originele KB veldjes


    val id = metaFields("idno").replaceAll(":ocr$", "")

    val concordances:List[Concordance] = snipper(textQuery, (doc \\ "text").head.asInstanceOf[Elem], mainMeta)
    //println(c)
    val snippets = Concordance.toHTMLList(concordances)

    val linkje = s"http://www.delpher.nl/nl/kranten/view?coll=ddd&identifier=$id&query=" + textQuery.toQueryString()


    val fullDoc =
      <div>
        <a target="_blank" href={linkje}>Toon in delpher</a><br/>
        <a onClick={s"toggleDiv('$id')"} style="text-decoration:underline">toon/verberg artikel</a>
        <div id={id} style="display:none">{doc}</div>
      </div> // .toString.replaceAll("\\s+", " ")

    
    val highlightedDoc = Highlighting.highlight(fullDoc, s => zipje.queryTerms.exists(t => s.toLowerCase.contains(t.toLowerCase)))
    val docField = highlightedDoc.toString.replaceAll("\\s+", " ") 

    val fields = articleMetaFields ++ metaFields ++ parseBibl(profile) ++ List("snippet" -> snippets, "document" -> docField, "link" -> linkje)

    val cols = columns.indices.map(i => postProcess(columns(i), fields.getOrElse(columns(i), "").toString   ))
    val l = cols.mkString("\t").replaceAll("\\\\", "&#92;").replaceAll("<title/>", "")
    l
  }

  def postProcess(fieldName: String, fieldContent: String): String = fieldContent

  def convertLine(line:String, config: ColumnConfiguration):Array[Any] =
  {

    val fields:Array[String] = line.split("\\t")
    val text = fields(config.textColumn)

    val id = fields(config.idColumn).replaceAll(".*urn=","")

    val linkje = s"http://www.delpher.nl/nl/kranten/view?coll=ddd&identifier=$id&query=" + config.query

    val link = <a href={linkje}>{fields(config.idColumn)}</a>
    val s = BasicTextProfiler.profile(text)



    val snippets = Concordance.toHTMLList(KBKwic.concordance(SingleTerm(config.query), text)) // dit werkt dus niet voor diamonds

    val nf = fields.updated(config.idColumn, link).updated(config.textColumn,snippets) ++ List(s("N"), s("p"), s("lang"))


    nf
  }

  def convertToHTML(inputFile: String, config: ColumnConfiguration) =
  {
    val convertedLines = scala.io.Source.fromFile(inputFile)
      .getLines()
      .map(l => convertLine(l,config))
      .toList
      .sortBy(c => c(c.size-1).asInstanceOf[String])
    val HTML =
      <html>
        <head><meta http-equiv="content-type" content="text/html;charset=utf8"></meta></head>
        <body>
          <table border="border" style="border-collapse:collapse; border-style: solid; border-width:1pt">
            {convertedLines.map(l => <tr valign="top">{l.map(x => <td>{x}</td>)}</tr>)}
          </table>
        </body>
      </html>
    HTML
  }
}

object TSVWriter
{
  def main(args: Array[String]):Unit =
  {
    val w = TSVWriter(args(0), Seq("idno", "pubYear_from", "titleLevel2",
      "titleLevel1", "genre", "profile.lexiconCoverage", "language", "snippet", "document"))
    if (args.size > 1) Console.err.println(w.makeLoadQuery(args(1)))
    w.printCSV()
  }
}
