package corpusprocessing.eindhoven
import java.io.File
import java.nio.file.Paths

import utils.PostProcessXML

import scala.xml._
import utils.ProcessFolder

object metadata {

}

/*
We hebben de VU-versie van het corpus, met de bestanden van Van Halteren: die partijen staan erbij en correct.
Maar het oorspronkelijke corpus bestond uit het corpus samengesteld door  P.C. Uit
den Boogaart, E.D. de Jong, A.M. van Mierlo. De VU heeft daar het Renkemacorpus aan toegevoegd, heeft alles opgeschoond, en Van Halteren is daar ook nog eens overheen gegaan.

Wat moet er gebeuren:
Bij alle titles moet, behalve voor CAMB de drie bovengenoemde samenstellers worden toegevoegd.
Bij CAMB moet J. Renkema toegevoegd worden.
Mathieu: kun jij checken of we die namen al in de Thesaurus hebben van personen? Anders Boukje even vragen om te helpen.
 */
object doubleFields
{


  import PostProcessXML._

  def someOtherPostfixes(d: Elem) =
  {
    val d1 = updateElement(d, _.label=="w", e => e.copy(attributes = e.attributes.filter(a => a.key != "sic" && a.key != "corr" && a.key != "maybenot")))
    val d2 = updateElement(d1, _.label=="teiHeader", e => updateElement(e, _.label=="s", e => e.copy(label = "p")))
    val d3 = updateElement(d2, _.label == "notesStmt", e => e.copy(child = <note/>))
    d3
  }

  def fixDoubles(d: Elem) =
    updateElement(d,
      e => (e.label == "bibl") &&
        Set("1960", "1970").forall(y =>
          (e \ "interpGrp").exists(
            i => (i \ "@type").text == "witnessYearLevel1_from" && (i \ "interp").text == y)),
      e => e.copy(child = e.child.filter(c =>
        ! ((  c \ "@type").text == "witnessYearLevel1_from" && (c \ "interp").text == "1970"))))


  def fixFile(in: String, out: String) =
  {
    val d = someOtherPostfixes(pidFix.fixPids(in)(fixDoubles(XML.load(in))))
    XML.save(out, d, "UTF-8")
  }

  def doFile(f: java.io.File):List[(String,Int)] =
  {
    val d = XML.loadFile(f)
    val x = (d \\ "interpGrp").map(i => (i \ "@type").toString()).toList.groupBy(identity).mapValues(_.size).toList
    //Console.err.println(x)
    val doubles = x.filter(_._2 > 1)
    if (doubles.nonEmpty) {
      Console.err.println(s"Double interpGrps in ${f.getCanonicalPath}: $doubles")
      val offendingFields = doubles.map(_._1)
      offendingFields.foreach(f => {
        val content = (d \\ "interpGrp").filter(i => (i \ "@type").toString == f)
        Console.err.println(content.map(c => (c \\ "interp").text))
      })
    }
    x
  }

  def main(args: Array[String]): Unit = {
    def it(s: String) =s
    // val allFields = ProcessFolder.processFolder( new java.io.File(args(0)), doFile).flatten
    ProcessFolder.processFolder(new java.io.File(args(0)), new java.io.File(args(1)), fixFile )
    //allFields.foreach(println)
  }
}

object checkDoubleFields
{
  import doubleFields._
  def main(args: Array[String]): Unit = {
    def it(s: String) =s
    val allFields = ProcessFolder.processFolder( new java.io.File(args(0)), doFile).flatten
    allFields.foreach(println)
  }
}

/*
E.D. de Jong : 813eba06-011b-11e4-b0ff-51bcbd7c379f
P.C. uit den Boogaart : 7f8f0166-011b-11e4-b0ff-51bcbd7c379f
A.M. van Mierlo : cd31daea-3540-a4b3-8de4-77dd70c6acdc
J. Renkema : 8086ea84-011b-11e4-b0ff-51bcbd7c379f

 */

object pidFix
{
  val extraKnakkers = <interp n="7f8f0166-011b-11e4-b0ff-51bcbd7c379f">P.C. Uit den Boogaart</interp>
    <interp n="813eba06-011b-11e4-b0ff-51bcbd7c379f">E.D. de Jong</interp>
    <interp n="cd31daea-3540-a4b3-8de4-77dd70c6acdc">A.M. van Mierlo</interp>

  val renkema = <interp n="8086ea84-011b-11e4-b0ff-51bcbd7c379f">J. Renkema</interp>


  def uuid(pid: String, title: String):String =
  {
    val source = pid + "." + title
    val bytes = source.getBytes("UTF-8")
    java.util.UUID.nameUUIDFromBytes(bytes).toString
  }

  def fixPid(f: String)(b: Elem):Elem = {
    //Console.err.println(b)

    def fixEditor(g: Elem) =
    {
      val typ  = (g \ "@type").text
      val extra = if (typ == "editorLevel3") extraKnakkers ++ renkema else
      if (f.toLowerCase.contains("camb")) renkema else extraKnakkers
      g.copy(child = g.child ++ extra)
    }

    val file = new java.io.File(f)
    val fileName = file.getParentFile.getName + "::" + file.getName
    val idno = ((b \ "idno").filter(i => (i \ "@type").text == "pid")).text.trim
    val title = (b \ "interpGrp").filter(i => (i \ "@type").text == "titleLevel1").text.trim
    val newUUID = uuid(idno,fileName)
    Console.err.println(s"$idno $fileName s$title $newUUID")

    val extra = <interpGrp type="pid"><interp>INT_{newUUID}</interp></interpGrp>

    val b0 = b.copy(child = extra ++ b.child.filter(c => !(c.label == "idno")))
    PostProcessXML.updateElement(b0, x => x.label == "interpGrp" && (x \ "@type").text.contains("ditor"), fixEditor)
  }

  def fixPids(f: String)(d: Elem) = {
    val d1 = PostProcessXML.updateElement(d, _.label == "bibl", fixPid(f))
    val pid = (d1 \\ "interpGrp").filter(i => (i \ "@type").text == "pid").text.trim
    PostProcessXML.updateElement(d1, n => (n.label == "idno") && (n \ "@type").text == "pid", x =>  <idno type="pid">{pid}</idno> )
  }
}

object repairWordIds
{
  val fallbackDir = Eindhoven.outputDir.replaceAll("/[^/]*/?$","/") + "xml-tussenresultaat-2018-05-04/"

  val baseXML  = Paths.get(Eindhoven.xmlDir).toAbsolutePath

  def relative(f:File) = baseXML.relativize(Paths.get(f.getCanonicalPath)).toString

  Console.err.println(fallbackDir)

  def findBuddy(f: java.io.File) =
  {
    val r = relative(f)
    new java.io.File(fallbackDir + r)
  }

  import Eindhoven.getId


  def fixIds(in: String, out:String) =
  {
    val fin = new java.io.File(in)
    val buddy = findBuddy(fin)

    val din = XML.loadFile(fin)
    val dbuddy = XML.loadFile(buddy)

    val words = (din \\ "w").map(_.asInstanceOf[Elem])
    val buddywords = (dbuddy \\ "w").map(_.asInstanceOf[Elem])

    val cwords = words.filter(w => (w \ "@corresp").nonEmpty)
    val cbuddywords = buddywords.filter(w => (w \ "@corresp").nonEmpty)

    val badIds = words.map(w => getId(w).get)
    val goodIds = buddywords.map(w => getId(w))

    val d1 = if (badIds.size == goodIds.size)
      {
        val idMap = badIds.zip(goodIds).filter(_._2.isDefined).toMap

        // Console.err.println(idMap)

        def setId(w: Elem) =
        {
          val id = getId(w).get
          if (idMap.get(id).isDefined)
            {
              val newId:String = idMap.get(id).get.get
              val w1 = w.copy(attributes=w.attributes.append( new UnprefixedAttribute("original_id", newId , Null)))
              w1
            } else w
        }
        PostProcessXML.updateElement(din, _.label=="w",  setId)
      } else
      {
        Console.err.println("Balen!!!: word count mismatch in " + in)
        //System.exit(1)
        din
      }

     XML.save(out,d1, "UTF-8")
  }


  val a0 = Eindhoven.xmlDir
  val a1 = Eindhoven.baseDir + "/xml-with-original-word-ids"
  def main(args: Array[String]): Unit = {
   // ProcessFolder.processFolder(new java.io.File(a0), new java.io.File(a1), fixIds)
  }
}
/*
Er zit nog een foutje in deze versie:
In de <bibl> hoort de pid-type net als de rest binnen een <interpGrp> te worden aangeboden, maar hier zit in een een <idno>.

Vergelijk:

CLUSIUS:
                        <interpGrp type="sourceID">
                            <interp>clusius:AA_G0002</interp>
                        </interpGrp>
                        <interpGrp type="pid">
                            <interp>INT_073ea83e-3c90-4b07-a161-d517064872e1</interp>
                        </interpGrp>

EINDHOVEN:
                        <interpGrp type="sourceID">
                            <interp>hvv-1-12-1-cdb</interp>
                        </interpGrp>
                        <idno type="pid">INT_009c9216-f924-4c05-b03a-99e0d9dd7ae6</idno>

 */
