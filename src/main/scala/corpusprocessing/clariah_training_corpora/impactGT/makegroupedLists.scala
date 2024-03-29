package corpusprocessing.clariah_training_corpora.impactGT

import scala.xml._
import java.io.File

case class DcMeta(d: Elem, f: File) {
  val dcx = (d \ "dcx").headOption.getOrElse(<bummer/>)

  lazy val fields = dcx.child.filter(_.isInstanceOf[Elem]).map(e => e.label -> e.text).toMap ++ Map("pageFileName" -> f.getCanonicalPath)

  val title = fields.getOrElse("title", "?")

  val date = fields.getOrElse("date", "?")

  val id = fields.getOrElse("recordIdentifier", "?")
  def key = s"$title--$date"
  override def toString = s"$id -- $title -- $date"
}

object makegroupedLists {
  import scala.xml._
  val pageDirEnhanced = "/mnt/Projecten/Corpora/Historische_Corpora/ImpactGT/Page/DDDEnhanced/"

  def main(args:Array[String])  = {

    val groups: Map[String, List[DcMeta]] = new File(pageDirEnhanced).listFiles.filter(_.getName.endsWith(".xml")).map(f => {
      val d = XML.loadFile(f)
      val meta = DcMeta(d,f)
      meta.key -> meta
    }).groupBy(_._1).mapValues(l => l.map(_._2).toList.sortBy(m => m.fields("pageFileName")))

    groups.foreach({
      case (k,l) => {
        val fields = l.head.fields
        val title = fields("title") + ":" + fields("date")

        val list = <pages group={title}>
          {l.map(x => <page>{x.fields("pageFileName")}</page>)}
        </pages>

        if (l.size > 1) println(list)

        val pw = new java.io.PrintWriter(s"/mnt/Projecten/Corpora/Historische_Corpora/ImpactGT/Page/DDDEnhanced/DDDGroupedLists/$title.xml")
        pw.print(list.toString)
        pw.close()
      }
    })
  }
}
