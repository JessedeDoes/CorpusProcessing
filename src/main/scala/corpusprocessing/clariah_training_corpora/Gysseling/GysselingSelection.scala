package corpusprocessing.clariah_training_corpora.Gysseling

import corpusprocessing.clariah_training_corpora.CLVN.{Doc, DocFromFile}
import corpusprocessing.clariah_training_corpora.patch_scripts.fixTokenization.getId

import java.io.File
import scala.xml.XML
import scala.xml._
import utils.PostProcessXML
import utils.ProcessFolder

object setAtt {
  def setAttribute(e: Elem, prefix: String, name: String, value: String): Elem = {
    val a = e.attributes.filter(_.key != name).append(new PrefixedAttribute(prefix, name, value, Null))
    e.copy(attributes = a)
  }

  def setAttribute(e: Elem, name: String, value: String): Elem = {
    val a = e.attributes.filter(_.key != name).append(new UnprefixedAttribute(name, value, Null))
    e.copy(attributes = a)
  }
}



object GysselingSelection {
  //val corpusURL = "https://corpora.ato.ivdnt.org/blacklab-server/CLVN"
  //val docsURL = "https://corpora.ato.ivdnt.org/blacklab-server/CLVN/docs/?number=3000"
  //val atHome = "/media/jesse/Data/Corpora/CLVN/Tagged"
  val baseDir = "/mnt/Projecten/Corpora/Historische_Corpora/CorpusGysseling/TeIndexeren/2020_07_31/" // /mnt/Projecten/Corpora/Historische_Corpora/CorpusGysseling/TDNEnhancedTagging/"

  def findAllXMlIn(path: String): Set[File]  = {
    val f = new File(path)
    if (f.isFile && f.getName.endsWith(".xml")) {
      Set(f)
    } else {
      val sub = f.listFiles().map(_.getCanonicalPath).toSet
      sub.flatMap(x => findAllXMlIn(x))
    }
  }

  // findAllXMlIn(atHome).iterator.map(_.getCanonicalPath).toStream
  // lazy val docsx: Seq[Doc] = XML.load(docsURL).flatMap(x => x \\ "doc").map(x => Doc(x,corpusURL))

  lazy val docs = util.Random.shuffle(findAllXMlIn(baseDir)).take(Integer.MAX_VALUE).map(DocFromFile)
  lazy val officialDocs = docs.filter(x => x.inputFile.matches(".*[01][0-9]{3}.*") && x.regionLevel1.nonEmpty)

  lazy val provinceGroups: Map[String, List[DocFromFile]] = {
    val a: Map[String, List[DocFromFile]] =
        officialDocs.groupBy(_.regionLevel1)
          .mapValues(l => l.filter(d => d.nTokens >= 200 && d.nTokens <= 3000).toList);
    a.mapValues(l => l.sortBy(_.nTokens))
  }

  lazy val toksPerProvince: Map[String, Int] = provinceGroups.map({case (p, l) => (p, l.map(_.nTokens).sum)}).filter(_._2 >= 100)
  lazy val survivingProvinces = toksPerProvince.keySet

  lazy val selection: Map[String, List[DocFromFile]] = provinceGroups.filter({case (x, _) => survivingProvinces.contains(x)}).mapValues(l => {
    var cumul = 0;
    l.takeWhile(d => {cumul = cumul + d.nTokens; cumul <= 6000})
  })


  def main(args: Array[String])  = {
    // docs16.foreach(x => println(x))
    println(toksPerProvince)
    println(toksPerProvince.size)

    println(s"We zouden moeten hebben: ${50000 / toksPerProvince.size} per provincie")
    toksPerProvince.foreach(println)
    val baseDir = new java.io.File("/tmp/GysSelectie/")
    baseDir.mkdir()
    val allTokens = selection.flatMap({case (p,l) =>
      println(s" ### $p: ${l.map(_.nTokens).sum} ### ")
      l.flatMap(d => {
        println(s"\t$d")
        d.save(baseDir.getCanonicalPath)
        d.tokens
      })
    })
    val tf = allTokens.groupBy(x => x).mapValues(_.size).toList.sortBy(-1 * _._2)
    val tfFile = new java.io.PrintWriter("/tmp/Gys_selectie_tf.txt")
    tf.foreach({case (w, c) => tfFile.println(s"$w\t$c")})
    tfFile.close()
  }
}

import GysselingSelection._
object GysselingLiterarySelection {
  val baseDir = "/mnt/Projecten/Corpora/Historische_Corpora/CorpusGysseling/TeIndexeren/2020_07_31/"
  lazy val docs: Set[DocFromFile] = util.Random.shuffle(findAllXMlIn(baseDir)).take(Integer.MAX_VALUE).filter(_.getName.matches("^[2-9].*")).map(DocFromFile)
  lazy val literaryDocs = docs.filter(x => !x.inputFile.matches(".*[2-9][0-9]{3}.*") && x.regionLevel1.nonEmpty)

  val outputFolder = "/mnt/Projecten/Corpora/Historische_Corpora/CorpusGysseling/GysLiteraireSelectie/"

  def stripContent(e: Node, x:NodeSeq): Node = {
    if (e.label.equals("teiHeader")) e
    else
      {
        if (e.label == "body") {
          if (x.head.label == "l") {
            e.asInstanceOf[Elem].copy(child = x)
          } else
            e.asInstanceOf[Elem].copy(child = <p>{x}</p>)
        } else if (e.isInstanceOf[Elem])
          e.asInstanceOf[Elem].copy(child = e.child.map(c => stripContent(c,x)))
        else e
      }
  }
  def produceDocAround(d: Elem, l: NodeSeq) = {
     stripContent(d, l)
  }
  def selectWordsIn(d: DocFromFile, max: Int): NodeSeq = {
    val doc = d.doc
    val lines = (doc \\ "l")
    val averageLineLength = lines.map(l => (l \\ "w").size).sum / lines.size.toDouble
    val maxLines = max / averageLineLength

    if (lines.size > 2 * maxLines) {
      val midden = (lines.size / 2) - maxLines / 2
      val selectedLines = lines.drop(midden.toInt).take(maxLines.toInt)
      selectedLines
    } else if (lines.size > 0) lines else {
      val words = doc.descendant.filter(n => Set("w","pc").contains(n.label))
      val midden = (words.size / 2) - max / 2
      words.drop(midden.toInt).take(max.toInt)
    }
  }

  def main(args: Array[String])  = {
    val total = docs.map(x => {

      val selectedNodes = selectWordsIn(x, 3600)
      val nWords = (selectedNodes \\ "w").size
      println(s"${x.inputFile}: selected: nodes ${selectedNodes.size} words ${nWords}, lines in doc ${(x.doc \\ "l").size })")
      val selectedPart = produceDocAround(x.doc, selectedNodes)
      XML.save(outputFolder + x.inputFile, selectedPart)
      nWords
    } ).sum
    println(total)
  }
}