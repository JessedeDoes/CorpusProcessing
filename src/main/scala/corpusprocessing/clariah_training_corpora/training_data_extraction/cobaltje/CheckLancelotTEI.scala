package corpusprocessing.clariah_training_corpora.training_data_extraction.cobaltje

import corpusprocessing.clariah_training_corpora.training_data_extraction.TrainingDataInfos
import corpusprocessing.eindhoven.Eindhoven.getId
import utils.zipUtils

import java.io.File
import java.nio.file.Files
import scala.xml._
object CheckLancelotTEI {
  val oldDownloadDir = "/mnt/Projecten/Corpora/TrainingDataForTools/CobaltExport/2025/download_19_februari_2025/"
  val newDownloadDir = "/data/Lancelot/download"
  val dir = "/mnt/Projecten/Corpora/TrainingDataForTools/CobaltExport/2025/download_19_februari_2025/Gys/LancelotExport/"
  val includeLegacyName = false


  def getJoin_n(n: Node): String =  {
    if ((n \ "join").nonEmpty)
      (n \ "join" \  "@n").text;
    else if ((n \ "@corresp").nonEmpty) { // oude gysseling en cobalt codering......
      val join_n = (List(getId(n).get) ++  (n \ "@corresp").text.split("\\s+").map(_.replaceAll("#","")).toList).sorted.mkString("_")
      join_n
    } else "" }

  def word(w: Node)  = if ( (w \ "seg").nonEmpty) (w \ "seg").text.trim else (w.text.trim)

  def words(d: Node) = (d \\ "w").map(word)

  def taggedText(d: Node) = words(d).mkString(" ")

  def uuid(s: String): String = {
    val bytes = s.getBytes("UTF-8")
    java.util.UUID.nameUUIDFromBytes(bytes).toString
  }


  def uuidFromContent(d: Node) = uuid(taggedText(d))

  def fixPoSforPunct(w: Elem): Elem = {
    val txt:String = if ( (w \ "seg").nonEmpty) (w \ "seg").text.trim else (w.text.trim);
    if (txt.matches("(\\p{P})+")) {
      //Console.err.println(s"Missed punctuation in $w")
      val w1 = corpusprocessing.parlamint.setAtt.setAttribute(w,"pos", "PC")
      w1.copy(label="pc")
    } else w
  }

  def fixJoinForW(w: Elem) = {
       if ( (w \ "join").nonEmpty || (w \ "@corresp").isEmpty )
         w else
         {
           val n = getJoin_n(w)
           // println(s"Fixing missing join, $n")
           w.copy(child = w.child ++ <join n={n}/>)
         }
    }

  def fixMissingJoins(d: Elem): Elem  = utils.PostProcessXML.updateElement(d, _.label=="w", fixJoinForW)
  def fixPunct(d: Elem) : Elem  = utils.PostProcessXML.updateElement(d, x => Set("w", "pc").contains(x.label), fixPoSforPunct)
  def fixWInInterp(d: Elem): Elem =  utils.PostProcessXML.updateElement(d, _.label=="interp", i => i.copy(child=Text(i.text)))

  val patches = List[Elem => Elem](fixMissingJoins, fixPunct, fixWInInterp)

  def patchDocument(d: Elem): Elem  = patches.foldLeft(d)({case (x,p) => p(x)})// fixPunct(fixMissingJoins(d))

  lazy val log = new java.io.PrintWriter("/tmp/logje.txt")

  def lookAtPidsAndFilenames(pathToZip: String, compare: Seq[((String,String), String)]= Seq()) = {

    log.println(s"\n\n##### $pathToZip #####")
    val m = compare.toMap
    val fileName = new File(pathToZip).getName
    lazy val paths = zipUtils.find(pathToZip)
    lazy val inputStreams = paths.map(p => (p.toString -> Files.newInputStream(p)))

    val id2name = inputStreams.sortBy(_._1).take(Integer.MAX_VALUE).map({case (n, s) => {
      val doc = XML.load(s)

      val citId = (doc \\ "cit").headOption.flatMap(getId).getOrElse("")
      val identifier = uuidFromContent(doc) + citId
      val speak = compare.nonEmpty
      //println(doc \\ "teiHeader")
      if (speak && false) {
        println(doc.copy(child = Seq()))
        println(doc.child.filter(_.isInstanceOf[Elem]).map(_.asInstanceOf[Elem].copy(child = Seq())))
        val idLikeElements = doc.descendant_or_self.map(n => getId(n)).filter(_.nonEmpty).take(4)
        println(s"${pathToZip.replaceAll(".*/", "")}: $n: $idLikeElements")
      }

      if (speak) {
        if (m.contains(fileName -> identifier))
          {
            log.println(s"Yep!!!? Nieuwe export: ${m(fileName -> identifier)} ~ Oude export $n?  ")
          } else {
          log.println(s"BoooHooHoo!!!? Oude export $n?")
        }
      }
      (fileName, identifier) -> n
    }})
    id2name.toList
  }

  def concat[T](l1: Seq[T], l2: Seq[T]) : Seq[T]  = l1 ++ l2

  def hasAmbiguity(m: Seq[((String,String), String)])  = {
     val problems = m.groupBy(_._1).values.filter(_.size > 1)
    problems.foreach(println)
    problems.nonEmpty
  }


  def main(args: Array[String]): Unit = {
    val trainingDataInfos = TrainingDataInfos.readFromFile(LancelotSettings.trainingDataInfoLocation)

     val newInfo: Seq[((String, String), String)] = new File(newDownloadDir).listFiles().filter(_.getName.endsWith(".zip")).map(f => {
       val zipName = f.getAbsolutePath
       val m: Seq[((String,String), String)] = lookAtPidsAndFilenames(zipName)
       m
     }).reduce(concat[((String,String), String)])

    println(s"Step 1: ${hasAmbiguity(newInfo)}")

    val oldInfo = new File(oldDownloadDir).listFiles().filter(_.getName.endsWith(".zip")).map(f => {
      val zipName = f.getAbsolutePath
      val m = lookAtPidsAndFilenames(zipName, newInfo)
      m
    }).reduce(concat[((String,String), String)])

    println(s"Step 2: ${hasAmbiguity(oldInfo)}")

    val mNew = newInfo.toMap // (set,identifier) -> name
    val oldInverse = newInfo.map({case ((set,id), name) => (set,name) -> id})
    val oldFile2NewFile: Map[(String, String), String] = oldInfo
      .map({case ((set,id), name) =>  (set,name) -> mNew((set,id))  }).toMap
      .map({case ((set,name), name2) => (set.replaceAll(".zip$", "").replaceAll("^cobalt_export_",""),name.replaceAll(".*/", "")) -> name2.replaceAll(".*/","")})
    oldFile2NewFile.take(10).foreach(println)


    val newInfos = trainingDataInfos.trainingDataInfos.map({case (set, ti) =>
      val s1 = set.replaceAll("cobalt_export_","").replaceAll(".zip$", "")
      println(s"Set: $s1")
      val relevant: Map[(String, String), String] = oldFile2NewFile.filter(_._1._1 == s1)
      val nameMap = relevant.map( {case ((set1,n1),n2) => (n1,n2)})
      nameMap.take(3).foreach(println)
      val newPartitions = ti.partitions.map({case (pName, pi) => {
          val newDocs = pi.documents.map(d => d.copy(sourceFileName = nameMap(d.sourceFileName),
            legacyName = if (includeLegacyName) Some(d.sourceFileName) else None))
          pName -> pi.copy(documents = newDocs)
      // println(nameMap)
      }})
      set -> ti.copy(partitions = newPartitions)
    })
    val newStuff = trainingDataInfos.copy(trainingDataInfos = newInfos)
    TrainingDataInfos.writeToFile(newStuff, "/tmp/stuffWithNewNames.json")
    log.close()
  }
}
