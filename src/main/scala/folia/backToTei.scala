package folia

import scala.xml._
import java.io._
import java.util.zip._

import folia.TDNTest.CGNForTDNTest.convert
import posmapping.{CGNPoSTagging, CHNStyleTags}

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

object BlacklabMeta {
  val metaFiles = new File(TDNTest.folia).listFiles.filter(_.getName.contains("meta")).iterator
  val docjes = metaFiles.flatMap(m  => {
    (XML.loadFile(m) \\ "doc").iterator
  })

  val docMeta = docjes.map(d => {
    val pid = (d \\ "docPid").text
    val fields = (d \ "docInfo").head.child.map(c => {
      c match {
        case e: Elem =>
          val name = e.label
          val values = (e \ "value").filter(v => !v.text.contains("Unspec")).map(_.text.trim)
          if (values.nonEmpty)
            <interpGrp type={name}>{values.map(v => <interp>{v}</interp> )}</interpGrp>
          else Seq()
        case _ =>  Seq()
      }

    })
    val bibl = <listBibl><bibl>{fields}</bibl></listBibl>
    pid -> bibl
  }).toMap
}

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
    override def convert(folia: Node):Node =
    {
      val id = folia \ s"${xml}id"
      val sentences = (folia \\ "s").map(convertSentence)
      val utterances = (folia \\ "utt").map(convertUtterance)
      <TEI.2 xml:id={id}>
        <teiHeader>{BlacklabMeta.docMeta(id.text)}
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

    override def convertWord(w: Node) =
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

      val newPoS = posMapping(pos)
      val t =  posmapping.CHNStyleTag(newPoS, posmapping.TagsetDiachroonNederlands.TDNTagset)
      val shredded = t.tagset.asTEIFeatureStructure(t)


      val teiw = if (cls =="PUNCTUATION" || pos.startsWith("LET"))
        Seq(<pc xml:id={id} msd={pos} type={newPoS}><seg>{content}</seg>{shredded}</pc>)
      else
        <w xml:id={id} msd={pos} pos={newPoS} lemma={lemma}><seg>{content}</seg>{shredded}</w><c> </c>


      val space = if ((w \ "@space").text.toString == "NO") Seq() else Seq(Text(" "))

      teiw ++ space
    }
  }

  def main(args: Array[String]): Unit = {
    val N = 1000 // Integer.MAX_VALUE
    val oldResult = new File(tei).listFiles()
    oldResult.foreach(_.delete())
    val foliaz = new File(folia).listFiles
      .filter(_.getName.endsWith(".xml"))
      .filter(!_.getName.contains("meta")).toList
    val shuffled = scala.util.Random.shuffle(foliaz)
    shuffled.take(N).foreach(f => {
      val d = XML.loadFile(f)
      val d1= CGNForTDNTest.convert(d)
      scala.Console.err.println(f.getName)
      XML.save(s"$tei/${f.getName}", d1, "UTF-8")
    })
  }
}