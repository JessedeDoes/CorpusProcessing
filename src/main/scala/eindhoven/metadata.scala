package eindhoven
import utils.PostProcessXML
import scala.xml._
import utils.ProcessFolder

object metadata {

}

object doubleFields
{

  import PostProcessXML._

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
    val d = pidFix.fixPid(fixDoubles(XML.load(in)))
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


object pidFix
{
  def fixPid(b: Elem):Elem = {
    val idno = ((b \ "idno").filter(i => (i \ "@type").text == "pid") \ "interp").text
    val extra = <interpGrp type="pid">
      <interp>{idno}</interp>
    </interpGrp>
    b.copy(child = extra ++ b.child.filter(c => !(c.label == "idno")))
  }

  def fixPids(d: Elem) = PostProcessXML.updateElement(d, _.label == "bibl", fixPid)
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
