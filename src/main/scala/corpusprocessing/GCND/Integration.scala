package corpusprocessing.GCND

import corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.grouping.groupWithFirst
import utils.alignment.{AlignmentGeneric, SimOrDiff}

import scala.xml._
import java.io.File
import java.util.Comparator


case class AlpinoToken(n: Node)  {
  val atts = n.attributes.map(n => n.key -> n.value.text).toMap
  val begin = atts("begin").toInt
}

case class AlpinoSentence(alpino: Elem)  {
  lazy val input_transcript = (alpino \\ "comment").text.replaceAll("^.*?\\|", "")
  lazy val id = ((alpino \\ "sentence").head \ "@sentid").text
  lazy val alpinoTokens = (alpino \\ "node").filter(x => (x \ "@word").nonEmpty).map(AlpinoToken).sortBy(_.begin)
}

/*
H036p_1_1       ja#t hé... gow... zeg weet je#t nog? a#me in#t eerste getrouwd waren da je drie dagen moest gaan gaan werken en de rest van de weke gaan doppen.
H036p_1_1_ndl   ja het hé... gow... zeg weet je het nog als we in het eerste getrouwd waren dat je drie dagen moest gaan gaan werken en de rest van de week gaan doppen.
TC              00:00:17.174 - 00:00:26.086
 */

object comp extends Comparator[(String,String)]
{
  import java.text.Normalizer

  def flatten(string : String) = Normalizer.normalize(string, Normalizer.Form.NFD).replaceAll("\\p{M}", "").replaceAll("[|*\\]\\[]","");

  override def compare(t: (String,String), t1: (String,String)):Int =
  {
    flatten(t._2).compareToIgnoreCase(flatten(t1._2))
  }
}

case class TranscriptSentence(lines: Seq[String], n: Int) {

  val a = new AlignmentGeneric(comp) // dit werkt niet...


  // Console.err.println(s"$tokenSequence // $wordSequence")

  def z(x: List[String]) = x.flatMap(_.split("nnnxnxnxnxnxnxn")).zipWithIndex.map({case (y,i) => (i.toString,y)})

  def align() {
    val o = z(orgTokens)
    val n = z(ndlTokens)
    val chunks: Seq[SimOrDiff[(String, String)]] = a.findChunks(o, n)

    val lr: Seq[(Boolean, List[(String, String)], List[(String, String)], Int)] = chunks.map(
      c => {
        //Console.err.println(c)
        val left = o.slice(c.leftStart, c.leftEnd)
        val right = n.slice(c.rightStart, c.rightEnd)
        (c.isSimilarity, left, right, c.leftStart)
      })
    lr.foreach(c => {
       val l = c._2.map(_._2).mkString(" ")
       val r = c._3.map(_._2).mkString(" ")
       println(s"${c._1 || c._2.size == c._3.size} $l <-> $r")
    })
  }

  val id = lines(0).replaceAll("\\s.*", "")
  val org = lines(0).replaceAll("^.*?\\s+", "").trim //  .replaceAll("\\s*\\.\\.\\.\\s*", " three_dots ").replaceAll("\\s+\\.", ".")
  val ndl = lines(1).replaceAll("^.*?\\s+", "").trim // .replaceAll("\\s*\\.\\.\\.\\s*", " three_dots ").replaceAll("\\s+\\.", ".")
  val tc = lines(2).replaceAll("^TC\\s+","")

  val orgTokens = org.split("\\s+|#").toList
  val ndlTokens = ndl.split("\\s+").toList

  val canAlign = orgTokens.size == ndlTokens.size

  override def toString: String = s"\n#####${id}--${n+1} (${orgTokens.size}, ${ndlTokens.size}) #####\norg=$org\ndl=$ndl\n$tc"
}

/*
TIER LINGUISTIC_TYPE_REF="vernederlandsing" PARENT_REF="H036p_1_1" TIER_ID="H036p_1_1_ndl">
        <ANNOTATION>
            <REF_ANNOTATION ANNOTATION_ID="a683" ANNOTATION_REF="a1">
                <ANNOTATION_VALUE>allez praat door.</ANNOTATION_VALUE>
            </REF_ANNOTATION>
        </ANNOTATION>
 */
case class Annotation(a: Node, typ: String) {
  val value = (a \\ "ANNOTATION_VALUE").text
  val id = ((a  \ "REF_ANNOTATION") \ "@ANNOTATION_ID").text
  val refId = ((a  \ "REF_ANNOTATION") \ "@ANNOTATION_REF").text
}

case class Package(elan: String, transcription: String, alpino_dir: String) {
  lazy val alpinoDocs = new File(alpino_dir).listFiles().filter(_.getName.endsWith(".xml")).iterator.map(XML.loadFile)
  lazy val alpinoSentences = alpinoDocs.map(AlpinoSentence)
  lazy val elanDoc = XML.load(elan)

  lazy val elanAnnotations = (elanDoc \\ "TIER").flatMap(t => (t \\ "ANNOTATION").map(a => Annotation(a, (t \ "@LINGUISTIC_TYPE_REF").text)))

  // lazy val elanSentences = (elanDoc \\ "TIER").filter(x => (x \ "@TIER LINGUISTIC_TYPE_REF").text == "vernederlandsing") // <TIER LINGUISTIC_TYPE_REF="vernederlandsing"

  def print() = {
    transcriptSentences.foreach(s => {
      println(s)
      if (!s.canAlign) s.align()
    })
    //alpinoSentences.foreach(x => println(x.id + " ===> " + x.input_transcript))
  }

  def parseFile(f: java.io.File): Seq[TranscriptSentence] = {
    val lines = io.Source.fromFile(f).getLines.toStream
    val language = f.getName.replaceAll("_.*","")
    val grouped = groupWithFirst[String](lines, x=> x.trim.isEmpty)

    val sentences = grouped.map(x => x.filter(_.nonEmpty)).filter(_.size == 3).zipWithIndex.map({case (x,i) => TranscriptSentence(x,i)})

    sentences
  }

  lazy val transcriptSentences = parseFile(new File(transcription))
}
// Q#H036p_1--H036p_1_2--0398
object Integration {
   val baseDir = "/mnt/Projecten/Hercules/Corpus-ZNL-Dialecten/Datasamples/Datasamples-completeworkflow/wetransfer_h036p_1-mp3_2023-03-31_1002/"
   val elan = baseDir + "H036p_1_AF.eaf"
   val transcription = baseDir +  "H036p_1_AF.txt"
   val alpino_dir = baseDir + "H036p_1--alpino/H036p_1--alpino/"

   lazy val test = Package(elan, transcription, alpino_dir)

   def main(args: Array[String]) = {
     test.print()
   }

}
