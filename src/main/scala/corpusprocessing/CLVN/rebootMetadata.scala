package corpusprocessing.CLVN

import deelBronType.weetNogNiet
import metadata._

import scala.xml._
import deelBronType._
import utils.PostProcessXML

import sys.process._
import CLVNUtils._

object o {
  def validateDirectory = {
  }


  val mcommand = "ls"

  //val output = (mcommand #< new java.io.File("/tmp/split.me") #> new java.io.File("/tmp/split")). !


  val findCommand = "find /mnt/Projecten/Nederlab/DiT-corpusproject-update/"

  //val findCommand = "ls /tmp"


  lazy val status = (findCommand #> new java.io.File("/tmp/aapje")).!

  lazy val allDirs =
  {
    status;
    scala.io.Source.fromFile("/tmp/aapje").getLines.toStream.filter(x => x.toLowerCase.contains("jesse") && (x.endsWith("Converted") || x.endsWith("TEI")))
  }


  def main(args: Array[String]) = {
    Console.err.println(allDirs.toList)
    allDirs.foreach(d => {
      val d1 = d.replaceAll(".*DiT-corpusproject-update/", "").replaceAll("[^A-Za-z0-9_-]", "_")
      val filesInD = new java.io.File(d).listFiles.toList.map(f => s"$f").mkString(" ")
      Console.err.println(filesInD)
      val validateCommand = s"jing data/tei_gtb.rng $filesInD"
      val status = (validateCommand #> new java.io.File(s"Log/$d1.validate.log")).!
      Console.err.println(s"$status voor $validateCommand")
    })
  }
}


object rebootMetadata {
  lazy val ditBase = new metabase(Settings.ditDevconfig)

  def getAlleBronnen(tableName: String): Map[String, Bron] = {
    val tableNamex = "cleaned_metadata"
    val mappingsx = List.empty[FieldMapping]
    ditBase.slurp(ditBase.allRecords(tableNamex)).map(m => Bron(m, mappingsx, List.empty, textBron, false)).groupBy(b => b.fields("idno")).mapValues(_.head)
  }


  lazy val alleBronnen: Map[String, Bron] = getAlleBronnen("cleaned_metadata")

  import utils.PostProcessXML._

  def uuid() = java.util.UUID.randomUUID().toString

  val tellertjes = database.DatabaseUtilities.Select(
    r => r.getInt("tellertje"),
    "cleaned_metadata"
  )

  def newIdnos = {
    ditBase.slurp(tellertjes).foreach(
      i => {
        ditBase.runStatement(s"update cleaned_metadata set idno = '${uuid()}' where tellertje=$i")
      }
    )
  }

  def idno(b: Elem) = ((b \\ "interpGrp").filter(n => (n \ "@type").toString == "idno") \ "interp").text.toString

  def rebootBibl(b: Elem): NodeSeq = {
    val btype = (b \ "@type").toString() match {
      case "textBron" => textBron
      case "divBron" => divBron
      case "milestoneBron" => milestoneBron
      case _ => textBron
    }

    val id_no = idno(b)

    val id = getId(b)
    if (id.isDefined && id.get.contains("range_"))
      {
        Console.err.println(s"Skipping superfluous bibl for $id $b")
        return NodeSeq.Empty
      }

    // Console.err.println(id_no + " from  " + b.toString)
    val bron = alleBronnen(id_no).copy(bronType = btype, finaal = true)

    if (btype == textBron) bron.bibl else <listBibl>{bron.bibl}</listBibl>
  }

  def unnestListBibl(b: Elem): NodeSeq = b.copy(child=b.child.filter(c => !(c.label == "listBibl"))) ++ (b.child.filter(_.label =="listBibl"))

  def unnestListBibls(d: Elem) = PostProcessXML.updateElement2(d, _.label=="listBibl", unnestListBibl).head.asInstanceOf[Elem]

  def splitListBibls(d: Elem):Elem = PostProcessXML.updateElement2(d,
    e => e.label == "listBibl" && (e \ "bibl").size > 1,
    e => e.child.map(c => <listBibl>{c}</listBibl>)).asInstanceOf[Elem]

  def getDocumentIdno(d: Elem) =
    (((d \\ "bibl").head \\ "interpGrp").filter(g => (g \ "@type").toString() == "pid").head \\ "interp").head.text

    //(d \\ "idno").head.text


  def newMetadata(d: Elem) = {
    updateElement2(d, _.label == "bibl", rebootBibl).asInstanceOf[Elem]
  }

  def removeAnaWithRange(e: Elem)  = e.copy(attributes = e.attributes.filter(a => !(a.key=="ana" && a.value.text.contains("range_")) ))

  def removeBadAna(d: Elem) = updateElement3(d, x => true, removeAnaWithRange)

  def checkFloatingText(d: Elem): Elem =
  {
    val floatingTexts = (d \\ "floatingText")
    val floatingTextsWithoutAna = floatingTexts.filter( f => (f \ "@ana").isEmpty)
    val extraBibls = if (floatingTextsWithoutAna.nonEmpty)
    {
      val idz0:Set[String] = floatingTextsWithoutAna.map(getId).filter(_.isDefined).map(_.get).toSet
      val idz = idz0.map(s => s.replaceAll("^INT_",""))
      val bronnenErbij = alleBronnen.values.filter(b => b.fields.contains("id_text_part") && idz.contains(b.fields("id_text_part"))).map(_.copy(bronType = divBron, finaal = true))
      bronnenErbij.map(_.bibl)
    } else Seq.empty
    if (extraBibls.nonEmpty)
      {
        // zet ze erbij na de main bibl
        val extraListBibl = <listBibl>{extraBibls}</listBibl>
        val d0 = updateElement5(d, e => ( e.label == "listBibl" && getId(e).nonEmpty && getId(e).get == "inlMetadata"), e => Seq(e,extraListBibl))
          .head.asInstanceOf[Elem]

        def linkFloatingText(f: Elem) =
          {
            val id = getId(f).get
            val biblId = "bibl_" + id
            val anaAtt = new UnprefixedAttribute("ana", "#" + biblId, Null)
            f.copy(attributes =  f.attributes.append(anaAtt))
          }

        updateElement(d0, floatingTextsWithoutAna.contains(_), linkFloatingText )
      } else d
  }

  def checkAnaResolutions(d: Elem, f: String)  =
  {

    val allBiblIds = (d \\ "bibl").map(getId).filter(_.isDefined).map(_.get).toSet
    val allAnaRefs = (d \\ "@ana").map(_.text).map(_.replaceAll("#","")).toSet

    if (allAnaRefs.nonEmpty)
      {
        Console.err.println(s"\n#### anarefs: ${allAnaRefs.size} in $f ####")
        val missingLinks = allAnaRefs.diff(allBiblIds)
        val uselesBibls = allBiblIds.diff(allAnaRefs)
        Console.err.println(s"Missing links: $missingLinks; unlinked Bibls: $uselesBibls")
      }
  }

  def process(infile: String, outfile: String) =
  {
    val doc = XML.load(infile)
    val doc1 =
      PostProcessXML.sequence(
        List(newMetadata, unnestListBibls, checkFloatingText, removeBadAna, splitListBibls ), doc)
    //val idno = getDocumentIdno(doc1)
    checkAnaResolutions(doc1, infile)
    XML.save(outfile, doc1, enc = "UTF-8")
  }

  def newMetadataForAll = { // neen, houd de directorystructuur, net als bij de parts
    val sizes = o.allDirs.map(
      d => {
        val dir = new java.io.File(d)
        val files = dir.listFiles().filter(f => f.getName.endsWith(".xml"))
        val withIdnos = files.map(f => {
          val doc = XML.loadFile(f)
          val doc1 =
            PostProcessXML.sequence(
              List(newMetadata, unnestListBibls), doc)
          val idno = getDocumentIdno(doc1)
          XML.save(s"/tmp/Dump/$idno.xml", doc1, enc = "UTF-8")
          (f.getCanonicalPath, idno)
        }).toSet
        (d, files.size, withIdnos.size, withIdnos.map(_._2).size)
      }
    )
    val total = sizes.map(_._2).sum
    Console.err.println(s"$total\n${sizes.mkString("\n")}")
  }

  //def main(args: Array[String]) = newMetadataForAll
  def main(args: Array[String]) = utils.ProcessFolder.processFolder(new java.io.File(args(0)), new java.io.File(args(1)), process)
}

object PidsIdsVoorStukjes
{
  import PostProcessXML._

  def uuidFrom(source: String): String =
  {
    val bytes = source.getBytes("UTF-8")
    java.util.UUID.nameUUIDFromBytes(bytes).toString
  }

  def process(infile: String, outfile: String) = {
    Console.err.println(infile)
    val d = XML.load(infile)
    val allBibls = (d \\ "bibl")
    val primaryPid = ((allBibls.head \ "interpGrp").filter(ig => (ig \ "@type").text == "pid").head \ "interp").text
    val subBibls = allBibls.tail

    val idMap:Map[String,String] = subBibls.map(b => { val id = getId(b); id.get -> uuidFrom(primaryPid + "." +  id.get) }).toMap

    def addPid(b: Elem):Elem =
    {
      val id = getId(b)
      if (id.isDefined) {
        val pidstukje = <interpGrp type="pid"><interp>{"INT_" + idMap(id.get)}</interp></interpGrp>
        val fieldsInB = (b \ "interpGrp")
        val otherStuffinB = b.child.filter(x => x.isInstanceOf[Elem] && x.label != "interpGrp")
        val newChildren  = Seq(pidstukje) ++ fieldsInB ++ otherStuffinB
        b.copy(child = newChildren)
      } else {
        Console.err.println(s"Geen xml:id voor $b!")
        b
      }

    }

    val d1 = updateElement(d,x => x.label == "bibl" && getId(x).isDefined && idMap.contains(getId(x).get), addPid)
    XML.save(outfile, d1, enc = "UTF-8")
  }

  def main(args: Array[String]) = utils.ProcessFolder.processFolder(new java.io.File(args(0)), new java.io.File(args(1)), process)
}

