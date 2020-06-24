package corpusprocessing.CGN

import java.io.File

import folia.{BlacklabMeta, FoliaToRudimentaryTEI}
import posmapping.TagsetDiachroonNederlands.tagsetFromSetOfTags

import scala.xml.{Node, Text, XML}

object CGNToTEIWithTDNTags {

  val mappingFile = "/mnt/Projecten/CLARIAH/CLARIAH-PLUS/Wp3/HistoricalDutch/Literature/Drive/cgn.tsv"
  val folia = "/home/jesse/Links/Werkfolder/Projecten/InterviewsCGN/PartOfCGN/"
  val tei = "/home/jesse/Links/Werkfolder/Projecten/InterviewsCGN/TEI/"
  lazy val mapping = io.Source.fromFile(mappingFile).getLines.map(l => {
    val x = l.split("\\t")
    x(1) -> x(2)
  }).toMap

  lazy val postProcessedMapping = tagsetFromSetOfTags("/tmp", "CGN", mapping)

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
    val N = Integer.MAX_VALUE
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
