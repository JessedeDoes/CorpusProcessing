package corpusprocessing.clariah_training_corpora.Gysseling

import corpusprocessing.clariah_training_corpora.CLVN.{Doc, DocFromFile}

import java.io.File
import scala.xml.XML



object GysselingSelection {
  //val corpusURL = "https://corpora.ato.ivdnt.org/blacklab-server/CLVN"
  //val docsURL = "https://corpora.ato.ivdnt.org/blacklab-server/CLVN/docs/?number=3000"
  //val atHome = "/media/jesse/Data/Corpora/CLVN/Tagged"
  val baseDir = "/mnt/Projecten/Corpora/Historische_Corpora/CorpusGysseling/TDNEnhancedTagging/"
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
    l.takeWhile(d => {cumul = cumul + d.nTokens; cumul <= 15000})
  })

  def main(args: Array[String])  = {
    // docs16.foreach(x => println(x))
    println(toksPerProvince)
    println(toksPerProvince.size)

    println(s"We zouden moeten hebben: ${100000 / toksPerProvince.size} per token")
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