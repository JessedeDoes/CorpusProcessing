package corpusprocessing.wolkencorpus.opendbnl

import utils.PostProcessXML

import scala.xml._
import java.io.{File, PrintWriter}
import scala.util.Random

object corpus17 {
  val opendbnl = "/mnt/Projecten/Corpora/Historische_Corpora/DBNL/PostProcessed"

  val allFiles = Random.shuffle(new File(opendbnl).listFiles.toList).iterator

  def getId(e: Node): String = e.attributes.filter(_.key == "id").headOption.map(_.value.text).getOrElse("UNK")

  def getMetadata(x : Elem)  = {
    val inlMetadata: Seq[Node] = (x \ "teiHeader" \\ "listBibl")
      .filter(l => (getId(l) == "inlMetadata" || (l \ "@type").text == "inlMetadata"))

    val interpGrps  = inlMetadata
      .toList
      .flatMap(l => l \\ "interpGrp")
    val meta = interpGrps.map(g => (g \ "@type").text -> ((g \ "interp").text + (g \ "interp" \ "@value").text)).toMap
    meta
  }

  def select(c: String)(f: File) = {
    val x = XML.loadFile(f)

    val meta = getMetadata(x)

    val yf = meta("witnessYearLevel2_from")
    val yt = meta("witnessYearLevel2_to")

    Console.err.println(s"${f.getName} $yf-$yt")

    if (yf.startsWith(c) && yt.startsWith(c)) {
      Console.err.println(s"Yep, selected ${f.getName} $yf-$yt ${meta("titleLevel2")}")
      true
    } else false
  }

  def main(args: Array[String]): Unit = {
     // allFiles.filter(select("16")).foreach(f => println(f.getName))
    val dumpie = new PrintWriter("/tmp/18.txt")
    allFiles.filter(select("17")).take(50).foreach(f => { dumpie.println(f.getName); dumpie.flush() } )
    dumpie.close()
  }
}

object bab2inlMetadata {

  val inDir = "/mnt/Projecten/Corpora/Historische_Corpora/BrievenAlsBuit/2.7CHN/"
  val outDir = "/mnt/Projecten/Corpora/Historische_Corpora/BrievenAlsBuit/2.7CHNMeta/"
  def interp(n:String,v: String) = <interpGrp type={n}><interp>{v}</interp></interpGrp>

  def fixDoc(d: Elem)  = {
    val meta = corpus17.getMetadata(d)
    println(meta.filter(_._2.nonEmpty))

    val newMeta = <listBibl xml:id="inlMetadata" type="inlMetadata">
      <bibl>
        {interp("titleLevel1",meta("title"))}
        {interp("authorLevel1",meta("author"))}
        {interp("witnessYearLevel1_from",meta("witnessYear_from"))}
        {interp("witnessYearLevel1_to",meta("witnessYear_to"))}
        {interp("author",meta("author"))}
        {interp("title",meta("title"))}
        {interp("corpusProvenance","Brieven als Buit")}
    </bibl>
    </listBibl>

    val d1 = PostProcessXML.updateElement5(d, _.label == "listBibl", lb => <listBibl type="bab">{lb.child}</listBibl> ++ {newMeta}).asInstanceOf[Elem]
    val d2 = PostProcessXML.updateElement(d1, _.label == "w", w => w.copy(child = (w \ "seg").head.child))
    d2
  }


  import utils.ProcessFolder

    def main(args: Array[String]): Unit = {
      ProcessFolder.processFolder(new File(inDir), new File(outDir), {case (i,o) =>
        if (i.endsWith(".xml")) {
          val g = new File(i)
          val inDoc = XML.loadFile(g)

          val outDoc = fixDoc(inDoc)
          val outTSV = o.replaceAll(".xml$", ".tsv")
          XML.save(o, outDoc, "UTF-8")
        }
      })

  }
}
