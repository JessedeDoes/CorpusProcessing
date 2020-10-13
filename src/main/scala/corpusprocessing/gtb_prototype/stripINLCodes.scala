package corpusprocessing.gtb_prototype
import scala.xml._
import utils.ProcessFolder._
import utils.PostProcessXML._

object stripINLCodes {

  def mapINLTag(e: Elem): NodeSeq = {
    val inpakken: NodeSeq => NodeSeq = e.label match {
      case "INL-i" => n => <hi rend="italic">{n}</hi>
      case "INL-u" => n => <hi rend="underline">{n}</hi>
      case "INL-b" => n => <hi rend="bold">{n}</hi>
      case "INL-got" => n => <hi rend="font-family:gothic">{n}</hi>
      case "INL-ptg" => n =>  {
        val size = "font-size:" +  (e \ "@nr").text
        <hi rend={size}>{n}</hi> }
      case "INL-sca" => n => <hi rend="text-transformation:smallcaps">{n}</hi>
      case "INL-tab1" => n => Seq(<lb/>) ++ n
      case "INL-tab2" => n => Seq(<lb/>) ++ n
      case "INL-tab3" => n => Seq(<lb/>) ++ n
      case "INL-witr1" => n => Seq(<lb/>) ++ n
      case "INL-witr2" => n => Seq(<lb/>) ++ n
      case "INL-witr3" => n => Seq(<lb/>) ++ n
      case "INL-jump" => n => Seq(Text(" ")) ++ n
      case "INL-center" => n => <hi rend="text=align:center">{n}</hi>
      case _ => n =>  {
        Console.err.println(e.label)
        n
      }
    }
    inpakken(e.child)
  }

  def processFile(in: String, out: String): Unit = {
    val doc = XML.load(new java.io.InputStreamReader(new java.io.FileInputStream(in), "iso-8859-1"))
    val outdoc = updateElement5(doc, e => e.label.startsWith("INL"), mapINLTag)
    XML.save(out, outdoc.asInstanceOf[Elem], "utf-8")
  }

  val _2TEI = new java.io.File("/mnt/Projecten/CLARIAH/CLARIAH-PLUS/Wp3/HistoricalDutch/Testbestanden/TEI-onverrijkt/GTB-prototype/2TEI")
  val TEI = new java.io.File("/mnt/Projecten/CLARIAH/CLARIAH-PLUS/Wp3/HistoricalDutch/Testbestanden/TEI-onverrijkt/GTB-prototype/TEI")

  def main(args: Array[String]): Unit = {
    processFolder(_2TEI, TEI, processFile, true, false)
  }
}
