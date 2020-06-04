package folia

import scala.xml._
import java.io._
import java.util.zip._

import folia.TDNTest.CGNForTDNTest.convert
import posmapping.CGNPoSTagging

case class FoliaToRudimentaryTEI(posMapping: String => String)
{
  val xml = "@{http://www.w3.org/XML/1998/namespace}"

  def convert(folia: Node):Node =
  {
    val id = folia \ s"${xml}id"
    val sentences = (folia \\ "s").map(convertSentence)
    val utterances = (folia \\ "utt").map(convertUtterance)
    <TEI.2 xml:id={id}>
      <teiHeader>
      </teiHeader>
     <text>
       <body>
        <div>
         <p>
           {sentences}
           {utterances}
         </p>
        </div>
       </body>
    </text>
    </TEI.2>
  } 

  def convertSentence(s: Node) = 
  {
    val id = s \ s"${xml}id"
    <s xml:id={id}>
      {(s \\ "w").map(convertWord)}
    </s>
  }


  def convertUtterance(s: Node) =
  {
    val id = s \ s"${xml}id"
    <l xml:id={id}>
      {(s \\ "w").map(convertWord)}
    </l>
  }

  def nonModernized(t:Node) = !((t \ "@class").toString == "contemporary")

  val preferedLemmaSet = "http://ilk.uvt.nl/folia/sets/frog-mblem-nl"
  val nextBestLemmaSet = "https://raw.githubusercontent.com/proycon/folia/master/setdefinitions/int_lemmatext_withcompounds.foliaset.ttl"



  def convertWord(w: Node) =
  {
    val content = (w \\ "t").filter(nonModernized).text
    val pos =  (w \\ "pos" \ "@class").toString()
    val lemmatags = w \\ "lemma"


    val lemmatag = lemmatags.find(t => (t \ "@set").toString == preferedLemmaSet)
      .getOrElse(lemmatags.find(t => (t \ "@set").toString == nextBestLemmaSet)
      .getOrElse(lemmatags.headOption.getOrElse(<lemma class="UNKNOWN"/>)))

    val lemma = (lemmatag \ "@class").toString

    val id = w \ s"${xml}id"

    //scala.Console.err.println(s"""$w, Lemma: ${w \\ "lemma"}""")

    val cls = (w \ "@class").toString

    val teiw = if (cls !="PUNCTUATION")
          <w xml:id={id} type={posMapping(pos)} lemma={lemma}>{content}</w>
       else
          <pc xml:id={id} type={posMapping(pos)}>{content}</pc>
    
    val space = if ((w \ "@space").text.toString == "NO") Seq() else Seq(Text(" "))
 
    Seq(teiw) ++ space
  }

  def main(args: Array[String])
  {
     val folia = XML.load(new GZIPInputStream(new FileInputStream(args(0))))
     println(convert(folia))
  }
}

object NederlabToCobalt extends FoliaToRudimentaryTEI(p => CGNPoSTagging.simplePoS(p))

object TDNTest {

  val mappingFile = "/mnt/Projecten/CLARIAH/CLARIAH-PLUS/Wp3/HistoricalDutch/Literature/Drive/cgn.tsv"
  val folia = "/home/jesse/Links/Werkfolder/Projecten/InterviewsCGN/PartOfCGN/"
  val tei = "/home/jesse/Links/Werkfolder/Projecten/InterviewsCGN/TEI/"
  lazy val mapping = io.Source.fromFile(mappingFile).getLines.map(l => {
    val x = l.split("\\t")
    x(1) -> x(2)
  }).toMap

  object CGNForTDNTest extends FoliaToRudimentaryTEI(p => {
    val p1 = mapping.getOrElse(p,"UNK")
    if (p1 == "UNK") scala.Console.err.println(p)
    p1
  }) {

  }
  def main(args: Array[String]): Unit = {
    val foliaz = new File(folia).listFiles
      .filter(_.getName.endsWith(".xml"))
      .filter(!_.getName.contains("meta"))
    foliaz.foreach(f => {
      val d = XML.loadFile(f)
      val d1= CGNForTDNTest.convert(d)
      scala.Console.err.println(f.getName)
      XML.save(s"$tei/${f.getName}", d1, "UTF-8")
    })
  }
}