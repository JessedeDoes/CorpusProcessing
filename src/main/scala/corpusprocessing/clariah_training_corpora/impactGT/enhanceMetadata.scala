package corpusprocessing.clariah_training_corpora.impactGT

import corpusprocessing.clariah_training_corpora.impactGT.enhanceMetadata.KBMeta

import scala.xml._
import java.io.{File, PrintWriter}
object enhanceMetadata {

  val pageDir = "/mnt/Projecten/Corpora/Historische_Corpora/ImpactGT/Page/DDD/" // SelectiePages/"
  val pageDirEnhanced = "/mnt/Projecten/Corpora/Historische_Corpora/ImpactGT/Page/DDDEnhanced/Page/"
  "https://services.kb.nl/mdo/oai?verb=GetRecord&identifier=DDD:ddd:010040741:mpeg21&metadataPrefix=didl"

  lazy val files: Iterator[(String, Elem)] = new File(pageDir).listFiles().iterator.filter(_.getName.endsWith(".xml")).map(x => x.getName -> XML.loadFile(x))

  case class KBMeta(identifier: String, fileName: String) {

    val id2 = identifier.replaceAll("DDD_", "ddd:").replaceAll("_[0-9]+$", ":mpeg21")
    val link = s"https://services.kb.nl/mdo/oai?verb=GetRecord&identifier=DDD:${id2}&metadataPrefix=didl"


    lazy val resp = XML.load(link)

    lazy val dcx: Node = {
      try {
        val dcxElem = (resp \\ "dcx").head
        dcxElem
      } catch {
        case e: Exception =>
          e.printStackTrace()
          println(s"error loading $link")
          <bummer/>
      }
    }

    lazy val fields = dcx.child.filter(_.isInstanceOf[Elem]).map(e => e.label -> e.text).toMap ++ Map("id" -> id2, "pageFileName" -> fileName)

    val title = fields.getOrElse("title","?")

    val date = fields.getOrElse("date","?")

    override def toString = s"$id2 -- $title -- $date"
  }



  case class PageMeta(e: Elem) {
    val fields: Map[String, String] = (e \\ "meta").map(m => {
      (m \ "@key").text -> m.text
    }).toMap

    val originalPath: String = fields("Original.Path")

    val id: String = originalPath.replaceAll(".*/", "").replaceAll("_(master|access).[a-z0-9]*$", "")

    def pad(x: String): String = if (x.length == 1) "0" + x else x
    val year: String = fields("Publication.Date.Year")
    val month: String = pad(fields.getOrElse("Publication.Date.Month","?"))
    val day: String = pad(fields.getOrElse("Publication.Date.Day","?"))

    val date = s"$year-$month-$day"
    val title: String = fields("Title")

    override def toString = s"$id -- $title -- $date"
    def toTSV = s"$id\t$title\t$date"
  }

  def getMetaFromPage(n: String, d: Elem) = {
    val comment = (d \\ "Metadata" \\ "Comment").text.replaceAll("&times;", "x").replaceAll("&", "&amp;")

    val d1 = try {
      val pageMeta = PageMeta(XML.loadString(comment))
      if (pageMeta.id.startsWith("DDD") && pageMeta.date >= "17") {

        val kbMeta = KBMeta(pageMeta.id,n)

        if (kbMeta.date != pageMeta.date) {
          println(s"\n###### $n #######")
          println("Page meta: " + pageMeta)
          println("KB meta:" + kbMeta)
        }

        val d1 = d.copy(child = kbMeta.dcx +: d.child)
        XML.save(pageDirEnhanced + n, d1)
        d1
      } else d
    } catch {
      case e:Exception => e.printStackTrace()
        d
    }
  }

  def main(args: Array[String]): Unit = {
    files.foreach({case (n,x) => getMetaFromPage(n,x)})
  }
}

/*
<srw_dc:dcx>
<dc:title>Het nieuws van den dag : kleine courant</dc:title>
<dc:identifier xsi:type="dcx:PPN">83249562X</dc:identifier>
<dc:date>1872-01-05</dc:date>
<dcterms:temporal>Dag</dcterms:temporal>
<dcx:recordRights>Koninklijke Bibliotheek Den Haag</dcx:recordRights>
<dc:publisher>Steendrukkerĳ Roeloffzen en Hübner;NV De Kleine Courant</dc:publisher>
<dcterms:spatial>Regionaal/lokaal</dcterms:spatial>
<dc:source>KB c226</dc:source>
<dcx:issuenumber>559</dcx:issuenumber>
<dcx:recordIdentifier>ddd:000017807:mpeg21</dcx:recordIdentifier>
<dc:identifier>http<meta key="Title">Arnhemsche courant</meta>://resolver.kb.nl/resolve?urn=ddd:000017807:mpeg21</dc:identifier>
<dc:type xsi:type="dcterms:DCMIType">Text</dc:type>
<dc:language xsi:type="dcterms:ISO639-1">nl</dc:language>
<dcterms:isPartOf xsi:type="dcx:collectionIdentifier">DDD</dcterms:isPartOf>
<dcterms:isPartOf>Databank Digitale Dagbladen</dcterms:isPartOf>
<ddd:yearsDigitized>1870 t/m 1914</ddd:yearsDigitized>
<dcterms:spatial xsi:type="dcx:creation">Amsterdam</dcterms:spatial>
<dcterms:issued>1870-1923</dcterms:issued>
</srw_dc:dcx>

<meta key="Publication.Date.Day">18</meta>
<meta key="Publication.Date.Month">12</meta>
<meta key="Publication.Date.Year">1817</meta>
<meta key="Title">Arnhemsche courant</meta>
<meta key="Title">Arnhemsche courant</meta>

 */

// <meta key="Original.Path">Ground truth images/Dutch Prints Online (Old Books)/113/dpo_113_0003_master.tif</meta>
