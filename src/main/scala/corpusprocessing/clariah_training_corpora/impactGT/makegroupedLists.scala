package corpusprocessing.clariah_training_corpora.impactGT

import scala.xml._
import java.io.File

case class DcMeta(d: Elem, f: File) {
  val dcx = (d \ "dcx").headOption.getOrElse(<bummer/>)

  lazy val fields = dcx.child.filter(_.isInstanceOf[Elem]).map(e => e.label -> e.text.trim).toMap ++ Map("pageFileName" -> f.getCanonicalPath)

  val title = fields.getOrElse("title", "?")

  val date = fields.getOrElse("date", "?")

  val id = fields.getOrElse("recordIdentifier", "?")
  def key = s"$title--$date"
  override def toString = s"$id -- $title -- $date"
}

object makegroupedLists {
  import scala.xml._
  val pageDirEnhanced =  "/mnt/Projecten/Corpora/Historische_Corpora/ImpactGT/SelectieCorrected/PageEnhanced//" // "/mnt/Projecten/Corpora/Historische_Corpora/ImpactGT/Page/DDDEnhanced/Page/"
  val outputDir = "/mnt/Projecten/Corpora/Historische_Corpora/ImpactGT/SelectieCorrected/GroupedLists/" // "/mnt/Projecten/Corpora/Historische_Corpora/ImpactGT/Page/DDDEnhanced/DDDGroupedLists/"
  def main(args:Array[String])  = {

    val groups: Map[String, List[DcMeta]] = new File(pageDirEnhanced).listFiles.filter(_.getName.endsWith(".xml")).map(f => {
      val d = XML.loadFile(f)
      val meta = DcMeta(d,f)
      meta.key -> meta
    }).groupBy(_._1).mapValues(l => l.map(_._2).toList.sortBy(m => m.fields("pageFileName")))

    groups.foreach({
      case (k,l) => {
        val fields = l.head.fields
        if (fields.contains("title")) {
          val title = fields("title").trim + ":" + fields("date").trim
          val titleEscaped = title.replaceAll("[^A-Za-z0-9:_]", "_")
          val list = <pages group={title}>
            {l.map(x => <page>
              {x.fields("pageFileName")}
            </page>)}
          </pages>

          if (l.size > 1) println(list)

          val pw = new java.io.PrintWriter(s"$outputDir/$titleEscaped.xml")
          pw.print(list.toString)
          pw.close()
        } else {
          Console.err.println("Geen titel (en dus vermoedelijk geen dcMeta): " + l)
        }
      }
    })
  }
}
