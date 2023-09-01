package corpusprocessing.clariah_training_corpora.CLVN

import scala.collection.immutable
import scala.xml._

case class Doc(n: Node) {
  val e  = n.asInstanceOf[Elem]
  def getValues(name: String): Seq[String] = ((n \\ "docInfo").head.child.filter(x => x.label == name).map(x => {  (x  \\ "value").text}))
  lazy val pid = (n \\ "docPid").text
  lazy val province = getValues("province").mkString("|")
  lazy val witness_year_from = getValues("witness_year_from").mkString("|")
  lazy  val witness_year_to  = getValues("witness_year_to").mkString("|")
  lazy val nTokens = (n \\ "lengthInTokens").text.toInt
  override def toString  = s"$pid ($nTokens tokens): $province, $witness_year_from-$witness_year_to"

  def getContent() = {
    val url = s"https://corpora.ato.ivdnt.org/blacklab-server/CLVN/docs/$pid/contents"
    XML.load(url)
  }

  def save(baseDir: String) = {
    val fName = baseDir + "/" + pid  + ".xml";
    XML.save(fName, getContent(), enc="UTF-8")
  }
}

object Selection {
   val docsURL = "https://corpora.ato.ivdnt.org/blacklab-server/CLVN/docs/?number=3000"
   lazy val docs: Seq[Doc] = XML.load(docsURL).flatMap(x => x \\ "doc").map(Doc)
   lazy val docs16 = docs.filter(x => x.witness_year_from >= "1500" && x.witness_year_to <= "1600").filter(_.province.nonEmpty)
   lazy val provinceGroups = docs16.groupBy(_.province).mapValues(l => l.filter(d => d.nTokens >= 200 && d.nTokens <= 2000)).mapValues(l => l.sortBy(_.nTokens))
   lazy val toksPerProvince = provinceGroups.map({case (p, l) => (p, l.map(_.nTokens).sum)}).filter(_._2 >= 5000)
   lazy val survivingProvinces = toksPerProvince.keySet
   lazy val selection = provinceGroups.filter({case (x, _) => survivingProvinces.contains(x)}).mapValues(l => {
     var cumul = 0;
     l.takeWhile(d => {cumul = cumul + d.nTokens; cumul <= 2750})
   })
  def main(args: Array[String])  = {
    // docs16.foreach(x => println(x))
    println(toksPerProvince)
    println(toksPerProvince.size)
    println(25000 / toksPerProvince.size)
    val baseDir = new java.io.File("/tmp/CLVNSelectie/")
    baseDir.mkdir()
    selection.foreach({case (p,l) =>
       println(s" ### $p: ${l.map(_.nTokens).sum} ### ")
       l.foreach(d => {
         println(s"\t$d")
         d.save(baseDir.getCanonicalPath)
       })
    })
  }
}
