package bab_aanvulling
import java.util

import database.DatabaseUtilities.AlmostQuery
import database._
import org.skife.jdbi.v2.Query

import collection.JavaConverters._
import scala.collection.mutable
import scala.xml._
import java.io.File
import scala.util.{Try,Success,Failure}

object metadata {

  val metaQuery = """select
                       brief.*,
                       coalesce(trans_bestand_corr,trans_bestand) as bestand_key,
                       relatieA.type as relatie_a, relatieB.type as relatie_b
                    from
                       brief left join relatie relatieA on brief.afz_rel_tot_adr_a = relatieA.code
                       left join relatie relatieB on brief.afz_rel_tot_adr_b = relatieB.code;
                    """

  val maanden = "unknown januari februari maart april mei juni juli augustus september oktober november december".split("\\s+")

  val relevantFields: Set[String] = """pid
  datum_jaar  datum_maand datum_dag
  type_brief autograaf signatuur
  adr_naam adr_naam_norm adr_loc_plaats_norm adr_loc_land_norm adr_loc_regio_norm adr_loc_schip_norm
  afz_naam afz_naam_norm afz_geslacht afz_klasse afz_geb_lftcat afz_rel_tot_adr
  regiocode afz_loc_plaats_norm afz_loc_land_norm afz_loc_regio_norm afz_loc_schip_norm""".split("\\s+").toSet


  def tableDump(t: String): List[mutable.Map[String, AnyRef]] = {
    val allMetadataQuery: AlmostQuery[util.Map[String, AnyRef]] = db => Settings.metaDB.handle.createQuery(metaQuery)
    val allMetadata: List[mutable.Map[String, AnyRef]] = Settings.metaDB.slurp(allMetadataQuery).map(_.asScala)
    allMetadata
  }

  lazy val brief: List[mutable.Map[String, AnyRef]] = tableDump("brief") // corpus = 3 of 4

  lazy val briefMetadataMap: Map[String, mutable.Map[String, AnyRef]] =
    brief.filter(m => true || m("corpus") == 3 || m("corpus") == 4).map(m => keys.keyFromMetadataRecord(m) -> m).toMap

  lazy val corpora: List[mutable.Map[String, AnyRef]] = tableDump("corpus")


  def makeMetadata(m: Option[mutable.Map[String, AnyRef]], images: List[String]): Elem = {
    <listBibl id="inlMetadata"><bibl>
    {if (m.isDefined) {
        val h = m.get
        val jaar1 = h.getOrElse("datum_jaar","unknown")
          val jaar = if (jaar1 != null) jaar1.toString.replaceAll(".*([0-9]{4}).*", "$1") else "unknown"

        val maand = Try(maanden(h("datum_maand").toString.toInt)) match {
          case Success(s) => s
          case _ => "[ ]"
        }

        h("afz_rel_tot_adr") = h("relatie_a")

        val dag0 = h("datum_dag")
        val dag = if (dag0 == null || dag0.toString == "" || dag0.toString.toLowerCase.contains("null")) "?" else dag0.toString
        val author = h("afz_naam_norm").toString
        val pid = "bab" +  h("code").toString

        <interpGrp type="pid"><interp value={pid}/></interpGrp><interpGrp type="datering"><interp value={s"$dag $maand $jaar"}/></interpGrp>
          <interpGrp type='title'><interp value={s"To ${h("adr_naam_norm")}, $dag $maand $jaar"}/></interpGrp>
          <interpGrp type='author'><interp value={author}/></interpGrp>
          <interpGrp type='witnessYear_from'><interp value={jaar}/></interpGrp>
          <interpGrp type='witnessYear_to'><interp value={jaar}/></interpGrp> ++
        h.filter({case (k,v) => relevantFields.contains(k)}).map( { case (k,v) => { // Console.err.println(s"$k -> $v");
          val v1 = if (v == null) "unknown" else v
          val v2 = translateMetadataValues.replaceValue(k, v1.toString)
          <interpGrp type={k}><interp value={v2}/></interpGrp>}} ) } }
      <interpGrp type="images">{images.map(i =>
          {
               val i1 = i.replaceAll(".*uploaden.","")
               val i2 = i1.replaceAll("DANS_(1[78])E-foto's CORR", "$1")
              <interp value={i2}/>
          })}</interpGrp>
    </bibl></listBibl>
  }

  def addMetadata(d: Elem, m: Option[mutable.Map[String, AnyRef]], images: List[String]): Elem =
  {
    import utils.PostProcessXML._
    updateElement(d, e => e.label =="teiHeader", e => e.copy(child = makeMetadata(m,images)))
  }


  object keys {
    def keyFromMetadataRecord(m: mutable.Map[String, AnyRef]): String = {
      val b = m("bestand_key").asInstanceOf[String]
      b.replaceAll("-TR.*", "").replaceAll("-def.*", "").replaceAll("_def.*", "").replaceAll("[^A-Za-z0-9_.-]", "_")
    }

    def keyFromFilename(sourceFile: String): List[String] = {
      val k0 = sourceFile.replaceAll(".*/", "").replaceAll(".[^/.]*$", "").replaceAll("[^A-Za-z0-9_.-]", "_")
      val k1 = k0.replaceAll("(^|[^0-9])0([0-9]{3})", "$1$2")
      if (k0 != k1) List(k0, k1) else List(k0)
    }

    def keyFromImageFilename(imageFile: String): String = {
      imageFile.replaceAll(".(jpg|JPG|png|PNG)$", "").replaceAll("[Ii][Mm][Gg]_", "").replaceAll("[- ]", "_")
    }

    def keyFromImageReference(imageFile: String): String = {
      imageFile.replaceAll(".(jpg|JPG|png|PNG)$", "").replaceAll("[Ii][Mm][Gg]_", "").replaceAll("[- ]", "_")
    }
  }

  import keys._

  lazy val allImages: List[(String, String)] = Settings.allImageFiles.filter(!_.getName.startsWith(".")).map(
    f => keyFromImageFilename(f.getName) -> f.getCanonicalPath
  ).toList

  lazy val imageMap: Map[String, String] = allImages.toMap

  def enhance(file: File): Unit = {
      val d = XML.loadFile(file)
      val images = (d \\ "image").map(_.text).toList
      val sourceFile = (d \\ "sourceFile").text

      val keys = keyFromFilename(sourceFile)


      val possibleMeta: Option[mutable.Map[String, AnyRef]] = keys.find(briefMetadataMap.contains(_)).map(x => briefMetadataMap(x))

      if (possibleMeta.isEmpty)
        println(s"[M] No metadata, Text filename=${sourceFile.replaceAll(".*/","")},  Keys tried=$keys")
      else if ( false && keys.size > 1)
       println(s"Metadata found, keys=$keys, trans_bestand=${(possibleMeta.get)("bestand_key")}")

      val imageList = images.flatMap(img =>
      {
        val imageKey = keyFromImageReference(img)
        val mtch: Option[String] = imageMap.get(imageKey)
        if (mtch.isEmpty) {
          println(s"[I] No image match, ImageKey=$imageKey, Image Reference=$img, Text Filename=${sourceFile.replaceAll(".*/", "")}")
          List()
        } else List(mtch.get)
      })

      val d1 = addMetadata(d, possibleMeta, imageList)
      XML.save(Settings.outputPath(file), d1, "UTF8")
      //println(images)
    }

  def main(args: Array[String]): Unit = {
    //println(allImages)
    //corpora.foreach(println)
   // brief.filter(m => m("corpus") == 3 || m("corpus") == 4).take(10000).foreach(m => Console.err.println(m("trans_bestand") + "-->"  + keyFromMetadataRecord(m)))
    Settings.allXMLFiles.foreach(enhance)
    // brief.foreach(println)
  }
}
