package corpusprocessing.clariah_training_corpora.CLVN

import corpusprocessing.clariah_training_corpora.patch_scripts.fixTokenization.getId
import corpusprocessing.metadata.jamaarCLVN
import utils.PostProcessXML

import java.io.File
import scala.collection.immutable
import scala.xml._

case class Doc(n: Node, corpusURL: String) {
  val e  = n.asInstanceOf[Elem]
  def getValues(name: String): Seq[String] = ((n \\ "docInfo").head.child.filter(x => x.label == name).map(x => {  (x  \\ "value").text}))
  lazy val pid = (n \\ "docPid").text
  lazy val province = getValues("province").mkString("|")
  lazy val witness_year_from = getValues("witness_year_from").mkString("|")
  lazy  val witness_year_to  = getValues("witness_year_to").mkString("|")
  lazy val decade = getValues("decade").mkString("|")
  lazy val inputFile = getValues(name ="fromInputFile").mkString("").replaceAll(".*/", "")
  lazy val nTokens = (n \\ "lengthInTokens").text.toInt
  override def toString  = s"$pid ($nTokens tokens): $province, $witness_year_from-$witness_year_to"

  def getContent(): Elem = {
    val url = s"$corpusURL/docs/$pid/contents"
    PatchUp.patchDoc(XML.load(url))
  }

  def save(baseDir: String) = {
    val fName = baseDir + "/" + inputFile // + pid  + ".xml";
    XML.save(fName, getContent(), enc="UTF-8")
  }
}

case class DocFromFile(f: File) {

  lazy val doc = XML.loadFile(f)
  lazy val toks = (doc \\ "w").size
  lazy val meta = (doc \ "teiHeader").head
  lazy val nTokens = toks



  lazy val e  = meta.asInstanceOf[Elem]
  def getValues(name: String): Seq[String] = (meta \\ "interpGrp").filter(g => (g \ "@type").text == name).flatMap(g => (g \\ "interp").map(_.text)) // ((n \\ "docInfo").head.child.filter(x => x.label == name).map(x => {  (x  \\ "value").text}))


  lazy val pid = (meta \\ "docPid").text
  lazy val province = getValues("province").mkString("|")
  lazy val regionLevel1 = getValues("witnessLocalization_regionLevel1").mkString("|")
  lazy val witness_year_from = getValues("witness_year_from").mkString("|")
  lazy val witness_year_to  = getValues("witness_year_to").mkString("|")
  lazy val witness_years_to: Seq[String] = getValues("witnessYearLevel1_to") ++ getValues("witnessYearLevel0_from")
  lazy val maxYearTo = if (witness_years_to.nonEmpty) witness_years_to.max else "undefined"
  lazy val periodOK = {val ok =  maxYearTo != "undefined" && maxYearTo < "1302";
    // Console.err.println(s"${f.getName}: tokens: $toks, waarvan met RES: $nRes, periodOK: $ok, $witness_years_to, resOK: $notTooManyRes");
    ok }


  lazy val nRes = (doc \\ "w").filter(x => (x \ "@pos").text.contains("RES")).size
  lazy val notTooManyRes = {
    val z = (20 * nRes < nTokens); Console.err.println(s"${f.getName}: tokens: $toks, waarvan met RES: $nRes, periodOK: $periodOK, $witness_years_to, resOK: $z")  ; z}
  lazy val decade = getValues("decade").mkString("|")
  lazy val inputFile = f.getName

  override def toString  = s"$pid ($nTokens tokens): $province, $witness_year_from-$witness_year_to"
  lazy val content = XML.loadFile(f)
  lazy val tokens = (content \\ "w").map(_.text)
  def getContent(): Elem =  content

  def save(baseDir: String) = {
    val fName = baseDir + "/" + inputFile // + pid  + ".xml";
    XML.save(fName, getContent(), enc="UTF-8")
  }
}

object CLVNSelection {
  val corpusURL = "https://corpora.ato.ivdnt.org/blacklab-server/CLVN"
   val docsURL = "https://corpora.ato.ivdnt.org/blacklab-server/CLVN/docs/?number=3000"
   val atHome = "/media/jesse/Data/Corpora/CLVN/Tagged"

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
   lazy val docsx: Seq[Doc] = XML.load(docsURL).flatMap(x => x \\ "doc").map(x => Doc(x,corpusURL))
   lazy val docs = findAllXMlIn(atHome).map(DocFromFile)
   lazy val docs16 = docs.filter(x => x.witness_year_from >= "1500" && x.witness_year_to <= "1600").filter(_.province.nonEmpty)
   lazy val provinceGroups: Map[String, List[DocFromFile]] =  { val a: Map[String, List[DocFromFile]] = docs16.groupBy(_.province).mapValues(l => l.filter(d => d.nTokens >= 200 && d.nTokens <= 2000).toList); a.mapValues(l => l.sortBy(_.nTokens)) }
   lazy val toksPerProvince: Map[String, Int] = provinceGroups.map({case (p, l) => (p, l.map(_.nTokens).sum)}).filter(_._2 >= 5000)
   lazy val survivingProvinces = toksPerProvince.keySet
   lazy val selection: Map[String, List[DocFromFile]] = provinceGroups.filter({case (x, _) => survivingProvinces.contains(x)}).mapValues(l => {
     var cumul = 0;
     l.takeWhile(d => {cumul = cumul + d.nTokens; cumul <= 2750})
   })

  def main(args: Array[String])  = {
    // docs16.foreach(x => println(x))
    println(toksPerProvince)
    println(toksPerProvince.size)
    println(25000 / toksPerProvince.size)
    val baseDir = new java.io.File("/tmp/CLVNSelectie/")
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
    val tfFile = new java.io.PrintWriter("/tmp/CLVN_Selectie_tf.txt")
    tf.foreach({case (w, c) => tfFile.println(s"$w\t$c")})
    tfFile.close()
  }
}

object CourantenSelection {
  val corpusURL = "http://svatkc10.ivdnt.loc/blacklab-server/couranten_ontdubbeld/"
  val docsURL = s"$corpusURL/docs?number=110000"

  lazy val docs: Seq[Doc] = XML.load(docsURL).flatMap(x => x \\ "doc").map(x => Doc(x,corpusURL))

  lazy val provinceGroups = docs.groupBy(_.decade).mapValues(l => l.filter(d => d.nTokens >= 200 && d.nTokens <= 2000)).mapValues(l => l.sortBy(_.nTokens))
  lazy val toksPerProvince = provinceGroups.map({case (p, l) => (p, l.map(_.nTokens).sum)}).filter(_._2 >= 2000)
  lazy val survivingProvinces = toksPerProvince.keySet
  lazy val selection: Map[String, Seq[Doc]] = provinceGroups.filter({case (x, _) => survivingProvinces.contains(x)}).mapValues(l => {
    var cumul = 0;
    l.takeWhile(d => {cumul = cumul + d.nTokens; cumul <= 2750})
  })

  def main(args: Array[String])  = {
    // docs16.foreach(x => println(x))
    println(toksPerProvince)
    println(toksPerProvince.size)
    println(25000 / toksPerProvince.size)
    val baseDir = new java.io.File("/tmp/CourantenSelectie/")
    baseDir.mkdir()
    selection.foreach({case (p,l) =>
      println(s" ### $p: ${l.map(_.nTokens).sum} ### ")
      l.foreach(d => {
        println(s"\t$d")
        d.save(baseDir.getCanonicalPath)
      })
    })
  }
}

object PatchUp {

  type B = (Int, Seq[Node])

  def breidUit(z: (B, Node)): B = {
    {
      z match  { case ((l, s), n) =>
        val (n1, l1) = renumberWords(n, l)
        (l1, s ++ Seq(n1))
      }
    }
  }

  def renumberWords(node: Node, k: Int) : (Node,Int) = {
    if (Set("w", "pc").contains(node.label)) {
      val id = getId(node)
      val n1: Node = node.asInstanceOf[Elem].copy(attributes = node.attributes.filter(x => x.key != "id").append(new PrefixedAttribute("xml", "id", Text(s"${node.label}.$k"), Null)))
      (n1,k+1)
    } else if (node.isInstanceOf[Elem]) {
      val t0: B = k -> Seq[Node]()
      val t: (Int, Seq[Node]) = node.child.toSeq.foldLeft(t0)({case (t:B,n:Node) => breidUit( (t,n))})
      node.asInstanceOf[Elem].copy(child = t._2) -> t._1
    } else {
      (node, k)
    }
  }

  private def renumber(e: Elem)  = renumberWords(e, 0)._1.asInstanceOf[Elem]

  def fixW(w: Elem): Seq[Node] = {

    def withText(t: String)  = w.copy(child={<seg>{t.trim}</seg>})
    if (w.toString().contains("<lb/>"))
      {
        val below = w.child.map(_.toString).mkString("").replaceAll("<lb[^<>]*/>", "LB_LB")
        val wordX = XML.loadString(s"<w>${below}</w>")
        val wordz = wordX.text.split("LB_LB").map(withText).toSeq
        println(wordz)
        wordz.zipWithIndex.flatMap({case (w,i) => if (i < wordz.size - 1) Seq(w, <lb has="flats"/>) else w}).toSeq
      } else if (w.text.matches(".*[a-zA-Z0-9].*,.*[a-zA-Z0-9].*"))
    {
      val wordz = w.text.split("\\s*,\\s*").map(withText)

      val metKomma: Seq[Node] = wordz.zipWithIndex.flatMap({case (w,i) => if (i < wordz.size - 1) Seq(w, <pc>,</pc>) else w}).toSeq
      metKomma
    } else withText(w.text);
  }

  val dir = "/mnt/Projecten/Corpora/Historische_Corpora/CLVN/CLVNSelectieTagged/"

  def patchDoc(d: Elem) = renumber(PostProcessXML.updateElement5(d, _.label=="w", fixW).asInstanceOf[Elem])
  def main(args: Array[String])  = {
    new File(dir).listFiles().filter(_.getName.endsWith(".xml")).foreach(f => {
      val d = XML.loadFile(f)
      val d1 =patchDoc(d)
      XML.save(dir + "Patched/" + f.getName, d1)
    })
  }
}
