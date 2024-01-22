package corpusprocessing.GCND

import scala.xml._
object telze
{
  def main(args: Array[String]) = {
    val doc = XML.load("/mnt/Projecten/Spelling/14_Deliverables/NTU2022-februari/spelling_2022_02_21.xml")
    val dimpies = (doc \\ "position").filter(x => (x \ "@label").text == "dim_ev")
    val n_echte_dimpies = dimpies.filter(x => (x \ "form").nonEmpty).size
    val n_entries = (doc \\ "entry").size
    val forms = (doc \\ "form").filter(f => (f \ "@approved").text != "false")
    println(s"entries: $n_entries, dim: $n_echte_dimpies, samen: ${n_entries + n_echte_dimpies}, forms: ${forms.size}")
  }
}
