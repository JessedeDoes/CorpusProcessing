package corpusprocessing.gysseling

import java.io.File

import scala.xml.{Elem, Node, XML}

object mapMiddelnederlandseTagsGys extends mapMiddelnederlandseTagsClass(true)
{
  val bronnenlijst = "/mnt/Projecten/Taalbank/Woordenboeken/VMNW/VMNWdb/Bronnenlijst/vmnw_bronnen.xml"

  lazy val bronMap: Map[String, (Node,Node)] = (XML.load(bronnenlijst) \\ "bron").toStream.map(
    b => (b \\ "docid").text -> {
      val b1 = XML.loadString("<bron><![CDATA[" + b.toString.trim + "]]></bron>")
      (b1,b)
    }
  ).toMap

  def addMetadataField(bibl: Elem, name: String, value: String): Elem =
  {
    // Console.err.println(s"$name = $value")
    bibl.copy(child = bibl.child ++ Seq(
      <interpGrp type={name}><interp>{value}</interp></interpGrp>
    ))
  }

  def addMeta(d: Elem, name: String, value: String): Elem =
  {
    updateElement(d, e => e.label=="listBibl" && getId(e).nonEmpty && getId(e).get == "inlMetadata", lb =>
      updateElement(lb, _.label == "bibl", x => addMetadataField(x, name, value) )
    )
  }

  def addMeta(d: Elem, extra: Map[String,String]): Elem =
  {
    updateElement(d, e => e.label=="listBibl" && getId(e).nonEmpty && getId(e).get == "inlMetadata", lb =>
      updateElement(lb, _.label == "bibl", x => extra.foldLeft(x)({ case (bibl, (n,v)) => addMetadataField(bibl,n,v)}) )
    )
  }


  def voegBronInfoToe(d: Elem): Elem = {
    val sourceID = ((d \\ "interpGrp").filter(g => (g \ "@type").text == "sourceID") \ "interp").text.replaceAll("corpusgysseling.","")
    val (brontxt, bronxml): (Node,Node) = bronMap.get(sourceID).getOrElse((<bron_not_found/>, <nee-echt-niet/>))

    val authenticity = (bronxml \\ "document" \\ "type").text
    val verblijfplaats = (bronxml \\ "document" \\ "verblijfplaats").text

    // Console.err.println(s"Authenticity: $authenticity")

    val m = Map("authenticityLevel1" -> authenticity, "signatureLevel1" -> verblijfplaats)

    updateElement(addMeta(d, m), _.label=="teiHeader", x => x.copy(child = x.child ++ Seq(brontxt)))
  }

  /*
     SOURCEDIR=/home/jesse/workspace/data-historische-corpora/gysseling/nederlab-tei
     TARGETDIR=/home/jesse/workspace/data-historische-corpora/gysseling/nederlab-enhanced-tei

   */

  override def main(args: Array[String]) = {
    val argz = if (args.size > 0) args else Array(GysselingSettings.SOURCEDIR, GysselingSettings.TARGETDIR)
    utils.ProcessFolder.processFolder(new File(argz(0)), new File(argz(1)), fixFile)
  }
}
