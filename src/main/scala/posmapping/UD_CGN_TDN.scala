package posmapping
import java.io.PrintWriter
import scala.collection.immutable
import scala.xml._

object UD_CGN_TDN {
   val folder = "/mnt/Projecten/Taalbank/UD-TDN-Lassy/Literatuur/"
   val mapping_ud_cgn_file = folder + "nl-cgn-uposf.html"
   lazy val mapping_xml_gcn_ud = utils.HTML.parse(new java.io.File(mapping_ud_cgn_file))
   val mapping_cgn_tdn_file = folder + "cgn_mapping_29_juni.html"
  lazy val mapping_xml_cgn_tdn = utils.HTML.parse(new java.io.File(mapping_cgn_tdn_file))


   def getOrElse(a: Seq[String], i:Int)  = {
     if (a.length > i) a(i) else "_"
   }

  lazy val cgn_ud=     (mapping_xml_gcn_ud \\ "tr").map(tr => {
     val cells = (tr \ "td").map(_.text.trim)
     cells(0) -> ("UPoSTag=" + getOrElse(cells,2).replaceAll("[)]","")   + "|"  + getOrElse(cells,3))
   }).toMap

  lazy val cgn_tdn  =  (mapping_xml_cgn_tdn \\ "tr").map(tr => {
    val cells = (tr \ "td").map(_.text.trim)
    (cells(0), cells(1), cells(2))
  })

  lazy val htmlTable =  {
    val rows = cgn_tdn.map({case (cgn,tdn,example) => {
      val ud = cgn_ud.getOrElse(cgn,"NOPE")
      <tr><td>{cgn}</td><td>{tdn}</td><td>{ud}</td><td>{example}</td></tr>

    } })
    <table>{rows}</table>
  }
  def main(args: Array[String]): Unit  = {
    val pwhtml = new PrintWriter("/tmp/out.html")
    pwhtml.println(htmlTable)
    pwhtml.close()
    val pw = new PrintWriter("/tmp/out.tsv")
    cgn_tdn.foreach({case (cgn,tdn,example) => {
      val ud = cgn_ud.getOrElse(cgn,"NOPE")
      pw.println(s"$cgn\t$tdn\t$ud\t$example")
    } })
    pw.close()
   // cgn_ud.foreach(x => println(x))


  }
}
