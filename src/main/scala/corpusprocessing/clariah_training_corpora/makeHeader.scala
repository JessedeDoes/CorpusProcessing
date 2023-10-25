package corpusprocessing.clariah_training_corpora
import utils.PostProcessXML

import scala.xml._
import java.io.{File, PrintWriter}

package object floep
{
  def getId(n: Node):String = n.attributes.filter(a => a.prefixedKey.endsWith(":id") || a.key.equals("id")).map(a => a.value.toString).headOption.getOrElse("?")
}

import floep._
import utils.PostProcessXML._
import org.json4s.DefaultFormats
import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.json4s.native.JsonMethods.parse
import org.json4s.native.Serialization.write
import java.io.File
/*
{"id":"168","tokens":["Ik","lees","voor","me","pleizier",",","meneer",",","als","ik","lees","."],"tags":["PD(type=pers,position=free)","VRB(finiteness=fin,tense=pres)","ADP(type=pre)","PD(type=poss,position=prenom)","NOU-C(number=sg)","LET","NOU-C(number=sg)","LET","CONJ(type=sub)","PD(type=pe
rs,position=free)","VRB(finiteness=fin,tense=pres)","LET"]}

 */

object readOldPartitions {
   val gtDir = "/home/jesse/workspace/nephomant/data/nederval/json"
   case class Sentence(id: String,
                       tokens: List[String],
                       tags: List[String],
                       xml_ids: List[String]  = List())
  implicit val formats = DefaultFormats

  def readFile(f: File) = io.Source.fromFile(f).getLines().map(l => {
    val p = parse(l)
    val s = p.extract[Sentence]
    //println(s.xml_ids)
    s
  }).toStream

  lazy val trainingSentences: Array[Sentence] = new File(gtDir).listFiles().filter(_.getName.contains("train")).flatMap(readFile)
  lazy val testSentences = new File(gtDir).listFiles().filter(_.getName.contains("test")).flatMap(readFile)

  lazy val trainWordIds = trainingSentences.flatMap(_.xml_ids).toSet
  lazy val trainSentenceKeys = trainingSentences.map(x => x.tokens.mkString(" "))

  lazy val testWordIds = testSentences.flatMap(_.xml_ids).toSet
  lazy val testSentenceKeys = testSentences.map(x => x.tokens.mkString(" "))

  def main(args: Array[String]) = {
    //println(testSentences)
    //println(testWordIds)
  }
}

import readOldPartitions._

object tei_2_json extends tei_to_huggingface_trait {
   // override def sample(sentences: Iterator[(tei_2_json.Sentence, Boolean)], sample_rate: Double, rate_test_train: Double): Iterator[(tei_2_json.Sentence, Boolean)] = sentences.map({case (s,b) => (s, s.partition.contains("test"))})

}

object toTSV {
  def toTSV(file: String, out: String)  = {
    val d = XML.load(file)
    val pw = new PrintWriter(out)
    (d \\ "s").foreach(s => {

      val wpc = (s.descendant.filter(x => Set("pc","w").contains(x.label)))
      val sent = wpc.map(_.text.trim).mkString(" ")
      pw.println(s"\n#${getId(s)}\n#$sent")
      wpc.foreach(n => pw.println(s"${n.text.trim}\t${n \ "@lemma"}\t${n \ "@pos"}\t${n \ "@n"}"))
    })
    pw.close()
  }
}

object makeHeader {
  val cleanForPublicRelease = true
  val corpora = "/home/jesse/workspace/galahad-corpus-data/public-corpora/clariah-evaluation-corpora/raw/"

  val corpusFiles = new File(corpora).listFiles.flatMap(d => d.listFiles())
  val dbnlVrij = "/mnt/Projecten/Corpora/Historische_Corpora/DBNL/PostProcessed/"
  val dbnlAll = "/mnt/DiskStation/homes/jesse/work/DBNL/MetMeta2021/"

  val dbnlFilesAll = new File(dbnlAll).listFiles.map(f => {
    val mainPart = f.getName.replaceAll(".meta.xml","")
    mainPart -> f
  }).toMap

  val dbnlFilesVrij = new File(dbnlVrij).listFiles.map(f => {
    val mainPart = f.getName.replaceAll(".meta.xml","")
    mainPart -> f
  }).toMap

  def addPartition(random: Boolean=false)(s: Elem): Elem = {
    val tokens = (s.descendant.filter(x => Set("pc","w").contains(x.label)))
    val sentenceKey = tokens.map(_.text.trim).mkString(" ")
    val idKey = getId(tokens.head)

    val partition = if (random) {
      if (Math.random() > 0.25) "train" else "test"
    } else
      (idKey, sentenceKey) match {

        case (i,s) if trainWordIds.contains(i) => "train"
        case (i,s) if testWordIds.contains(i) => "test"

        case (i,s) if trainSentenceKeys.contains(s) => "train"
        case (i,s) if testSentenceKeys.contains(s) => "test"
        case (i,s) => {
          println(s"Not found: [$sentenceKey]")
          "unknown"
        }
    }
    s.copy(attributes = s.attributes.append(new UnprefixedAttribute("ana", s"#$partition", Null)))
  }

  def processFile(f: File)  = {

     val d = XML.loadFile(f)

     val id = getId(d).replaceAll(".ids$", "")
     val gratis = dbnlFilesVrij.contains(id)

     // println(s"$id: ${gratis}")
     val dbnlFile = dbnlFilesAll(id)

     val location = f.getParentFile.getCanonicalPath.replaceAll("/raw", "")

     println("Location: " +  location)
     if (!gratis) println(s"$id : ${dbnlFile.getCanonicalPath}")
     val dbnlDoc = XML.loadFile(dbnlFile)
     val header = (dbnlDoc \\ "teiHeader").head.asInstanceOf[Elem]
     val metHeader = updateElement(d, _.label == "teiHeader", x => header)

     val cleaned = updateElement(metHeader, x => Set("pc","w").contains(x.label), w => {
       w.copy(attributes = w.attributes.filter(a => Set("id", "pos", "lemma", "n").contains(a.key)))
     })

     val partitioned = updateElement(cleaned, _.label=="s", addPartition(Set("set_16","set_15").exists(c => f.getCanonicalPath.contains(c))))

     val noTwo = partitioned.copy(label="TEI")
     val test = PostProcessXML.updateElement5(noTwo, _.label=="s", s => {
       if ((s \ "@ana").map(_.text).mkString.contains("test")) Seq(s) else Seq()
     }).asInstanceOf[Elem]

    val train = PostProcessXML.updateElement5(noTwo, _.label=="s", s => {
      if ((s \ "@ana").map(_.text).mkString.contains("train")) Seq(s) else Seq()
    }).asInstanceOf[Elem]

     val locationTest = s"$location/test/"

     val locationTrain = s"$location/train/"
     List(locationTest, locationTrain).foreach(new File(_).mkdir())

     XML.save(s"$location/$id.xml", noTwo, "UTF-8")
     XML.save(s"$locationTest/$id.test.xml", test, "UTF-8")
     XML.save(s"$locationTrain/$id.train.xml", train, "UTF-8")


     val iter: Iterator[(String, Elem)] = List(id -> noTwo).iterator

     tei_2_json.processDocuments(iter, s"$locationTrain/$id", sentence_element = "s")
     toTSV.toTSV(s"$locationTrain/$id.train.xml", s"$locationTrain/$id.train.tsv")
     toTSV.toTSV(s"$locationTest/$id.test.xml", s"$locationTest/$id.test.tsv")
  }

  def main(args: Array[String])  = {
    corpusFiles.foreach(processFile)
  }
}
