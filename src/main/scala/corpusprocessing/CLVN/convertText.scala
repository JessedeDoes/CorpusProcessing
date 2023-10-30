package corpusprocessing.CLVN

import java.io.FileWriter

import corpusprocessing.CLVN.enschede.rules4all
import utils.PostProcessXML
import utils.PostProcessXML._

import scala.xml._
import scala.util.matching._
import scala.util.{Failure, Success, Try}
import CLVNUtils._
case class Miskleun(f: String, errors:Set[String])

case class ParseResult(missingIds: Set[String], failures: Int, successes: Int, failedFiles:Set[Miskleun])
{
  def ++(p:ParseResult) = ParseResult(missingIds ++ p.missingIds, failures + p.failures, successes + p.successes,
    failedFiles ++ p.failedFiles)
  lazy val kleunen:String = failedFiles.map(k => s"\n\n\n\n####\nErrors in file: ${k.f}\n${k.errors.mkString("\n\n")}").mkString("\n")
  override def toString = s"""Missing record ids:\n${missingIds.mkString("\n")}\nfailed:$failures\nsuccess:$successes\n$kleunen"""
}

trait FlardParseResult
{

}

trait hasMain
{
  def main(args: Array[String])
}

case class FlardSuccess(id: String, elem: Elem) extends FlardParseResult

case class FlardFailure(id: String, fileName: String, exception: org.xml.sax.SAXParseException, toBeCorrected: String, preprocessed: String) extends FlardParseResult
{
  def errorMessage():String =
  {

    val line = exception.getLineNumber
    val column = exception.getColumnNumber
    val offendingLine = toBeCorrected.split("\\n")(line-1)
    val lineBelow = (0 until column).map(i => " ").mkString("") + "^"
    val context = s"""################# $fileName #####################################################################
$offendingLine
$lineBelow
#############################################################################################


          """

    s"""failure for text in file $fileName with id $id at line $line, column $column: ${exception.getMessage}"""
  }

  def correctionDoc =
    s"""
<errorDocument fileName="$fileName" id="$id">
<error>
<!--
      ${errorMessage()}
-->
</error>
      $toBeCorrected
</errorDocument>
     """
}

import metadata.cleanId

class TextParser(tableName: String, inputFolder: String, outputFolder: String, errorFolder: String, correctionFolder: String) extends hasMain
{
  lazy val correctedXML:Map[String,Elem] = new java.io.File(correctionFolder).listFiles()
    .filter(f => f.getName.endsWith(".xml"))
    .map(f => XML.loadFile(f))
    .map(e => (e \\ "text" \ "@id").toString -> (e \\ "text").head.asInstanceOf[Elem]).toMap

  val textPat:Regex = "(?s)<te(ks|x)t(.*?)</te(ks|x)t>".r
  val bracketPattern:Regex = "\\((.*?)\\)".r
  val restrictedBrackPattern:Regex = "\\(([a-zA-z]*?)\\)".r

  val squareBracketPattern:Regex = "\\[(.*?)\\]".r

  case class Header(fields: Array[String])

  val getIdPattern:Regex = "id\\s*=\\s*[\"'“”](.*?)[\"'“”]".r

  val folioPattern:Regex = "(?i)\\$\\$fol\\.?\\s*([0-9]+[rv])(\\.?)".r
  val milestonePattern:Regex = "<(/?)milestone-([be])\\s*id\\s*=\\s*([^<>\\s]*)\\s*>".r
  val apensuper:Regex  = "@(.*?)@".r

  val findFolder:Regex = "([0-9]{2}[^/]*)".r

  lazy val folder:String = findFolder.findFirstIn(inputFolder).get.replaceAll(" ", "_")

  def milestrone(s:String): String =
  {
    milestonePattern.replaceAllIn(s, m => s"""<milestone xml:id="${m.group(3)}-${m.group(2)}"/>""")
  }

  val desc = "desc=\"(.*?)\"".r

  def addGapExtent(g: String):String =
  {
    val descr = desc.findFirstMatchIn(g).map(_.group(1))
    if (descr.isDefined)
      {
        val d = descr.get.trim
        val words = "\\s+".r.findAllMatchIn(d).toList.size
        val chars = d.replaceAll("\\+s", "").length
        val g1 = g.replaceAll(">", s""" extent="$words words, $chars characters"""")
        Console.err.println(s"GAPPPPPP !$g1")
        //System.exit(1)
        g1
      } else g
  }
  def getId(txt: String):String = cleanId(getIdPattern.findFirstMatchIn(txt).map(_.group(1)).getOrElse("NotFound"))

  // ToDo voor Gelderland (en andere docjes) : vervang "xx </expan>" door "xx</expan> ", etc

  val dinges = "\u2028"

  val ruleName2Rule:Map[String, String=>String] = Map(
    "betterQuotes"
      -> (txt => txt.replaceAll("[“”]","\"")),
    "cleanId"
      -> (txt => getIdPattern.replaceAllIn(txt, m => s"""id="${cleanId(m.group(1))}"""")),
    "Deldel"
      -> (txt => txt.replaceAll("<Del>", "<del>").replaceAll("</Del>", "</del>")),
    "milestrone"
      -> milestrone,
    "folio"
      -> (txt => folioPattern.replaceAllIn(txt, m => s"<pb type='folio' n='${m.group(1)}'/>")),
    "pageBreaks"
      -> (txt => txt.replaceAll("//","<pb/>")),
    "lineBreaks"
      -> (txt => txt.replaceAll("([^<])/([^>])","$1<lb/>$2")), // staat altijd aan -- moet eigenlijk niet
    "tekxt"
      -> (txt =>  txt.replaceAll("<tekst","<text").replaceAll("/tekst>","/text>")),
    "rologap"
      -> (txt => txt.replaceAll("<>", "<gap resp='editor' extent='2 characters'/>")),
    "sillychars"
      -> (txt => txt.replaceAll("(?i)<(oe|ae|ye|ue|oe|ije|oe.|y.e|ra)>", "<hi rend='between_angular_brackets'>$1</hi>")), // deze twee regels in de ordeltotaal miserie
    "removebody"
      -> (txt => txt.replaceAll("<(/?)body.*?>", "")),
    "marges_alkmaar"
      -> (txt => txt.replaceAll("(?si)\\[in marge:([^\\[\\]]*?)\\]", "<marge>$1</marge>")),
    "expan_blank"
      -> (txt => txt.replaceAll(" +</expan>","</expan> ")),

    "002b" -> (txt => txt.replaceAll(dinges, " ")),

    "gaps"
      -> (txt => addGapExtent(txt.replaceAll("\\s*(\\.\\.\\.|…)(\\s+(\\.\\.\\.|…))*\\s*", //  …
      "<gap resp=\"editor\" desc=\"$0\" reason=\"illegible\"/>"))
      ),
    "brackets2expan"
      -> (txt => bracketPattern.replaceAllIn(txt,  m => s"""<expan resp="editor">${m.group(1)}</expan>""")),
    "brackets2expanbeperkt"
      -> (txt => restrictedBrackPattern.replaceAllIn(txt,  m => s"""<expan resp="editor">${m.group(1)}</expan>""")),
    "square2sup"
      -> (txt => squareBracketPattern.replaceAllIn(txt,  m => s"""<add place="above">${m.group(1)}</add>""")),
    "square2supplied"
      -> (txt => squareBracketPattern.replaceAllIn(txt,  m => s"""<supplied resp="editor">${m.group(1)}</supplied>""")),
    "apensuper"
      -> (txt => apensuper.replaceAllIn(txt,  m => s"""<add place="above">${m.group(1)}</add>"""))
  )

  val basicRules = List("betterQuotes", "cleanId", "Deldel", "pageBreaks", "lineBreaks", "tekxt", "milestrone", "rologap", "sillychars", "002b")

  val leuvenRules = List(
    "betterQuotes",
    "cleanId",
    "Deldel",
    "pageBreaks",
    "lineBreaks",
    "tekxt",
    "rologap",
    "sillychars",
    "milestrone",
    "folio",
    "gaps",
    "marges_alkmaar",
    "brackets2expan",
    "square2sup",
    "expan_blank",
    "apensuper",
    "removebody",
    "002b"
  )

  val rules4all:List[String] = leuvenRules.diff(List("brackets2expan", "square2sup", "apensuper", "gaps"))

  lazy val rulesToUse:List[String] = if (inputFolder.toLowerCase.contains("leuven")) leuvenRules else rules4all

  def parseFlard(txt: String, fileName: String):FlardParseResult =
  {
    // Console.err.println(s"Weird chars: ${weirdChars(txt)}")
    val id = getId(txt)
    val myTxt =
      if (correctedXML.contains(id))
      { Console.err.println(s"Corrected file for $id"); correctedXML(id).toString}
      else txt

    if ("<tekst".r.findAllMatchIn(txt).toStream.size > 1)
    {
      val id = getId(txt)
      Console.err.println(s"!!!!! Basic flard delimiting error in $fileName after id $id")
      System.exit(1)
    }

    val preprocessed =  rulesToUse.map(ruleName2Rule).foldLeft(myTxt)((txt, f) => f(txt))

    val t = Try(XML.loadString(preprocessed))
    t match {
      case Success(e:Elem) => FlardSuccess((e \ "@id").toString, e)
      case Failure(e: org.xml.sax.SAXParseException) =>
      {
        val toBeCorrected = basicRules.map(ruleName2Rule).foldLeft(txt)((txt, f) => f(txt))
        FlardFailure(getId(txt), fileName, e,toBeCorrected,preprocessed)
      }
    }
  }

  def weirdChars(txt: String):Set[String] =
  {
    txt.replaceAll("\\p{L}","").replaceAll("[0-9<>,=.!?;()\\[\\]:/-]","").replaceAll("\\s","").split("").toSet
  }

  val xmlns = "@{http://www.w3.org/XML/1998/namespace}"



  def success(rawDocument: Elem, bronnen: Map[String,Bron], bronnenByMilestone: Map[String,Bron]=Map.empty, f: String):Option[String] =
  {

    val id = rawDocument \ "@id"


    val postProcessedDocument = {
      import utils.PostProcessXML._
      sequence(Seq(flattenMilestones,
        updateGaps,
        renameMargeToAdd,
        ontWiebel,
        subTextToTEI,
        createBasicDocumentStructure,
        PeetjesInDivjes,
        supsToHi,
        fixIds0,
        cleanP,
        cleanPBs,
        fixMilestones,
        renameMargeToAdd,
        posIsPlace,
        floatingTextInQ),
        rawDocument)
    }



    def noBE(x:String)  = x.replaceAll("(?i)-?[be]$", "")

    def getSubtekstId(n: Node) = n.attributes.filter(a => a.prefixedKey.endsWith(":id") || a.key.equals("id"))
      .map(a => cleanId(s"$id.${a.value.toString}")).headOption

    val milestones = postProcessedDocument \\ "milestone"
    val milestoneIds = milestones.map(getSubtekstId)
      .filter(_.isDefined).map(_.get)
      .map(_.toString)
      .toSet
      .map(noBE)

    //Console.err.println(milestones.map(getId(_)))

    val milestoneBronnen = milestoneIds
      .map(id => bronnenByMilestone.get(id))
      .filter(_.isDefined).map(_.get)
      .toList.map(_.copy(bronType = deelBronType.milestoneBron))

    val divIds = ((rawDocument \\ "div") ++ (rawDocument \\ "subtekst") ++ (rawDocument \\ "back") ++ (rawDocument \\ "front"))
      .map(getSubtekstId)
      .filter(_.isDefined).map(_.get)
      .map(_.toString)

    //Console.err.println(s"DIV IDS: $divIds")

    val failedSubIds = (milestoneIds ++ divIds).filter(id => bronnenByMilestone.get(id).isEmpty)

    if (failedSubIds.nonEmpty) {
      failedSubIds.foreach(fsi => Console.err.println(s"!!Geen submetadata voor $fsi in  ${id.toString}, file=$f"))
      //Console.err.println(s"!!Geen submetadata gevonden voor ${id.toString}, items=<${failedSubIds.mkString("\n")}> in $f")
    }

    val divBronnen = divIds.map(id => bronnenByMilestone.get(id)).filter(_.isDefined).map(_.get).toList.map(_.copy(bronType = deelBronType.divBron)).toSet

    // Console.err.println(s"$milestoneIds -->  ${milestoneBronnen}")


    val nietGevondenBron = Bron(Map("record_id" -> s"not_found_in_metadatabase_$id"), metadata.minimalMapping)

    val bronOption:Option[Bron] =
      (if (id.toString.equals("dummydeMummy")) // als er alleen maar milestones zijn, geen goede situatie trouwens
        Some(Bron(Map("record_id" -> "dummy"), metadata.minimalMapping))
      else
        bronnen.get(id.toString)).map(b => b.copy(deelBronnen = milestoneBronnen ++ divBronnen))

    // ToDo: divs in front ....
    val bronOfNiks:Option[Bron]= Some(bronOption.getOrElse(nietGevondenBron))

    if (bronOfNiks.get.equals(nietGevondenBron))
      Console.err.println(s"!!Geen metadata gevonden voor ${id.toString} in $f")

    // ids van backs en fronts verdwijnen ...
    import utils.PostProcessXML._

    bronOfNiks match {
      case Some(b) =>
        val tei =
          <TEI xmlns:tei="http://www.tei-c.org/ns/1.0" xmlns="http://www.tei-c.org/ns/1.0">

              {b.header}

            {sequence(Seq(fixIds1), (postProcessedDocument))}
          </TEI>

        val pw = new java.io.PrintWriter(new java.io.FileWriter(s"$outputFolder/${folder}_${cleanId(id.toString)}.tei.xml"))
        pw.write(CLVNUtils.addAnas(tei).toString)
        pw.close()


      case None =>
      //println(s"!!!!! Metadata ontbreekt voor |$id|")
    }
    if (bronOption.isDefined) None else Some(id.toString)
  }


  lazy val alleMetadata:Map[String,Bron] = metadata.getAlleBronnen(tableName, metadata.leuvenseMapping) // Parametriseer of normaliseer mapping!

  Console.err.println(alleMetadata.get("dummy"))

  // System.exit(1)

  lazy val alleMetadataByMilestone:Map[String,Bron] = metadata.getDeelBronnenByPartKey(tableName, metadata.leuvenseMapping)
  // Zie: https://medium.com/@sinisalouc/overcoming-type-erasure-in-scala-8f2422070d20

  import scala.reflect.ClassTag

  def pick[A,T](list: Seq[A])(implicit tag: ClassTag[T]):Seq[T] = list.flatMap {
    case element: T => Some(element)
    case _ => None
  }

  def postConditional[T](test: T => Boolean, first: T, second: () => T):T =
  {
    val x = first
    if (test(x)) x else second()
  }

  def parseOneTextFile(f: String):ParseResult =
  {
    Console.err.println("parsing: " + f)
    val text = scala.io.Source.fromFile(f, "UTF-8").getLines.mkString("\n")

    val flardjes = postConditional[List[String]](
      test = l => l.nonEmpty,
      first = textPat.findAllMatchIn(text).map(m => m.group(0)).toList,
      second = () => List("<tekst id='dummy'>" + text + "</tekst>"))

    val flardResults:Seq[FlardParseResult] = flardjes.map(x => parseFlard(x,f))

    val failures = pick[FlardParseResult,FlardFailure](flardResults)
    val successes = pick[FlardParseResult, FlardSuccess](flardResults)

    Console.err.println(s"successfully parsed from $f: ${successes.size} text scraps")

    val missingIds = successes.map(s => success(s.elem, alleMetadata, alleMetadataByMilestone, f))
      .filter(_.isDefined)
      .map(_.get)


    val failureMessages = failures.map(_.errorMessage()).toSet
    val miskleunen = if (failures.nonEmpty) Set(Miskleun(f,failureMessages)) else Set.empty[Miskleun]

    failures.foreach(
      f =>
      {
        val fw  = new FileWriter(errorFolder + cleanId(f.id) + ".xml" )
        fw.write(f.correctionDoc)
        fw.close()

        val fw1  = new FileWriter(errorFolder + cleanId(f.id) + "preprocessed.xml" )
        fw1.write(f.preprocessed)
        fw1.close()
      }
    )
    ParseResult(missingIds.toSet, failures.size, successes.size, miskleunen)
  }

  def cleanOutputFolder = new java.io.File(outputFolder).listFiles().foreach(
    f =>
      {
        // Console.err.println(f.getCanonicalPath);
        f.delete()
      }
  )

  def main(args: Array[String]):Unit =
  {
    var dir = inputFolder
    Console.err.println(dir)
    cleanOutputFolder
    val allResults =  new java.io.File(dir).listFiles.map(f => parseOneTextFile(f.getCanonicalPath))
    val global = allResults.reduce(_ ++ _)
    val fw = new FileWriter(s"$errorFolder/$tableName.convert.log")
    fw.write(global.toString)
    fw.close()
  }
}



import Settings._

object delfland extends TextParser("meta", delflandDir + "TEKSTEN_CONV", delflandDir + "Converted", delflandDir + "CorrectMe/", delflandDir + "Corrected")
object elburg extends TextParser("meta", elburgDir + "TEKSTEN_CONV", elburgDir + "Converted", elburgDir + "CorrectMe/", elburgDir + "Corrected")


object alkmaar extends TextParser("meta", alkmaarDir + "TEKSTEN_CONV", alkmaarDir + "Converted", alkmaarDir + "CorrectMe/", alkmaarDir + "Corrected")

/* Enschede heeft haakjes voor afkortingen, maat niet alle haakjes zijn afkortingen */

object enschede extends TextParser("meta", enschedeDir + "TEKSTEN_CONV", enschedeDir + "Converted", enschedeDir + "CorrectMe/", enschedeDir + "Corrected") {
  override lazy val rulesToUse: List[String] = rules4all ++ List("brackets2expanbeperkt")
}

object zutphen extends hasMain
{
  private lazy val zutfies = Settings.zutphenSubsWithPlainText.map( z => {
    val d = Settings.zutphenBase + z
    new TextParser("meta", d + "TEKSTEN_CONV", d + "Converted", d + "CorrectMe/", d + "Corrected")
  })
  def main(args: Array[String]):Unit = zutfies.foreach(z => z.main(Array()))
}

object gelderland extends TextParser("meta", gelderDir + "TEKSTEN_CONV", gelderDir + "Converted", gelderDir + "CorrectMe/", gelderDir + "Corrected")
object gemert extends TextParser("meta", gemertDir + "TEKSTEN_CONV", gemertDir + "Converted", gemertDir + "CorrectMe/", gemertDir + "Corrected")
object bredevoort extends TextParser("meta", bredeVoortDir + "TEKSTEN_CONV", bredeVoortDir + "Converted", bredeVoortDir + "CorrectMe/", bredeVoortDir + "Corrected")


object leuven extends TextParser("leuven", leuvenDir + "TEKSTEN_CONV", leuvenDir + "Converted", leuvenDir + "CorrectMe/", leuvenDir + "Corrected")


// Peter van Os kan niet zomaar vanwege milestones bject enschede extends TextParser("meta", enschedeDir + "TEKSTEN_CONV", enschedeDir + "Converted", enschedeDir + "CorrectMe/", enschedeDir + "Corrected")

object petertje extends TextParser("meta", peterDir + "TEKSTEN_CONV", peterDir + "Converted", peterDir + "CorrectMe/", peterDir + "Corrected")

object seyger1 extends TextParser("meta", seyger1Dir + "TEKSTEN_CONV", seyger1Dir + "Converted", seyger1Dir + "CorrectMe/", seyger1Dir + "Corrected")
{
  override lazy val rulesToUse: List[String] = rules4all ++ List("brackets2expan", "square2supplied")
}

object seyger2 extends TextParser("meta", seyger2Dir + "TEKSTEN_CONV", seyger2Dir + "Converted", seyger2Dir + "CorrectMe/", seyger2Dir + "Corrected")

object ditToevoegingen extends TextParser("meta", ditToevoegingenDir + "TEKSTEN_CONV",
  ditToevoegingenDir + "Converted", ditToevoegingenDir + "CorrectMe/", ditToevoegingenDir + "Corrected")


object alles extends App
{
  val dingen:List[hasMain] = List(delfland,elburg,alkmaar,enschede,zutphen,gelderland,gemert,bredevoort,leuven,petertje,seyger1,seyger2,ditToevoegingen)

  dingen.foreach(d => d.main(Array()))
}



// verwer, foreest, de rijp, niedorp

// ToDo je krijgt als er al een body inzit dubbel body
