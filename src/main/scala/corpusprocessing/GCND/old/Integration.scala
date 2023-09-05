package corpusprocessing.GCND.old

import corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.conll_u._
import corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.grouping.groupWithFirst
import org.json4s.native.JsonMethods.parse
import org.json4s.{DefaultFormats, _}
import utils.alignment.{AlignmentGeneric, SimOrDiff}

import java.io.File
import java.util.Comparator
import scala.xml._



/*
    {
        "annotation_id": "a7",
        "end_time": 8930,
        "line": "ewel Telesphore dat is nu [ @skip ... ] binnen [ @skip ... ] de tiende van januari [ @skip ... ] dertig jaar zeker ?",
        "line_alpino": "H036p_1--H036p_1_1--0003|ewel Telesphore dat is nu [ @skip ... ] binnen [ @skip ... ] de tiende van januari [ @skip ... ] dertig jaar zeker ?",
        "line_id": "H036p_1--H036p_1_1--0003",
        "line_number": "0003",
        "participant_id": "H036p_1_1",
        "recording_id": "H036p_1",
        "start_time": 2760,
        "tier_id": "H036p_1_1_ndl"
    },
 */
case class SentenceInfo(
                                annotation_id: String,
                                end_time: String,
                                line: String,
                                line_alpino: String,
                                line_id: String,
                                line_number: String,
                                participant_id: String,
                                recording_id: String,
                                start_time: String,
                                tier_id: String
                       ) {

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
case class Annotation(a: Node, typ: String, p: Package) {
  val value = (a \\ "ANNOTATION_VALUE").text
  val id = if (a.label == "ALIGNABLE_ANNOTATION") (a  \ "@ANNOTATION_ID").text else ((a  \ "REF_ANNOTATION") \ "@ANNOTATION_ID").text
  val refId = ((a  \ "REF_ANNOTATION") \ "@ANNOTATION_REF").text
  lazy val refersTo = p.annotationMap.get(this.refId)
  lazy val referredValue = refersTo.map(_.value).getOrElse("_")
  val jsons = p.jsonInfos.filter(_.annotation_id == this.id)
  val alpinos = jsons.flatMap(j => p.alpinoMap.get(j.line_id).map(_.xml))

  lazy val xml = <annotation type={typ} xml:id={id}>
    <licht id={refId}>{referredValue}</licht>
    <zwaar>{value}</zwaar>
    <alpino_tokens>{"\n"}{alpinos}
    </alpino_tokens>
    <json_alpino>{jsons.map(j => j.line_alpino)}</json_alpino>
    <json>{jsons}</json>
  </annotation>
  // op deze manier vind je zinnetjes met ids als H036p_1--H036p_1_1--0013a niet
  // en wat is er met H036p_1--H036p_1_2--0412? Zit in H036p_1--H036p_1_2--0410--0412.xml
}

case class Package(elan: String, transcription: String, alpino_dir: String, json: String) {
  lazy val alpinoDocs = new File(alpino_dir).listFiles().filter(_.getName.endsWith(".xml")).iterator.map(XML.loadFile)
  lazy val alpinoSentences = alpinoDocs.map(AlpinoSentence)
  lazy val alpinoMap = alpinoSentences.toList.map(s => s.id -> s).toMap
  lazy val elanDoc = XML.load(elan)

  lazy val someAnnotations = (elanDoc \\ "TIER").flatMap(t => (t \\ "ANNOTATION").map(a => Annotation(a, (t \ "@LINGUISTIC_TYPE_REF").text, this)))
  lazy val alignableAnnotations = (elanDoc \\ "TIER").flatMap(t => (t \\ "ALIGNABLE_ANNOTATION").map(a => Annotation(a, "alignable", this)))
  lazy val elanAnnotations= someAnnotations ++ alignableAnnotations
  lazy val annotationMap = elanAnnotations.map(a => a.id -> a).toMap

  // lazy val elanSentences = (elanDoc \\ "TIER").filter(x => (x \ "@TIER LINGUISTIC_TYPE_REF").text == "vernederlandsing") // <TIER LINGUISTIC_TYPE_REF="vernederlandsing"

  def print() = {
    //jsonInfos.foreach(println)
    // println(alpinoMap.keySet)
    elanAnnotations.foreach(x => if (x.jsons.nonEmpty && x.typ == "vernederlandsing") {
      if (false) {
        println("\nid=" + x.id + " type=" + x.typ)
        println("value=" + x.value)
        println("refers to=" + x.referredValue)
        println(x.jsons)
        println(x.alpinos)
      }
      println(x.xml)
    })
    if (false)
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
  implicit val formats = DefaultFormats

  def parseJsonInfo(f: String) = {
    val content = io.Source.fromFile(f).getLines().mkString("\n")
    val parsed = parse(content)
    val infoz = parsed.extract[List[SentenceInfo]]
    infoz
  }

  lazy val jsonInfos = parseJsonInfo(json)
  lazy val transcriptSentences = parseFile(new File(transcription))
}
// Q#H036p_1--H036p_1_2--0398
object Integration {
   val baseDir = "/mnt/Projecten/Hercules/Corpus-ZNL-Dialecten/Datasamples/Datasamples-completeworkflow/wetransfer_h036p_1-mp3_2023-03-31_1002/"
   val elan = baseDir + "H036p_1_AF.eaf"
   val json  = baseDir + "H036p_1--alpino.json"

   val transcription = baseDir +  "H036p_1_AF.txt"
   val alpino_dir = "/home/jesse/Downloads/H036p_1--alpino/H036p_1--alpino/" // baseDir + "H036p_1--alpino/H036p_1--alpino/"

   lazy val test = Package(elan, transcription, alpino_dir, json)

   def main(args: Array[String]) = {
     test.print()
   }
}
