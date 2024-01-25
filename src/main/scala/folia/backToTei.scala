package folia

import scala.xml._
import java.io._
import java.util.zip._

import corpusprocessing.CGN.CGNToTEIWithTDNTags
import corpusprocessing.CGN.CGNToTEIWithTDNTags.CGNForTDNTest.convert
import posmapping.TagsetDiachroonNederlands.tagsetFromSetOfTags
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

    println(w)
    val content = (w \\ "t").filter(nonModernized).headOption.map(_.text).getOrElse("")
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
          <w xml:id={id} pos={posMapping(pos)} lemma={lemma}>{content}</w>
       else
          <pc xml:id={id} pos={posMapping(pos)}>{content}</pc>

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
  val metaFiles = new File(CGNToTEIWithTDNTags.folia).listFiles.filter(_.getName.contains("meta")).iterator
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

