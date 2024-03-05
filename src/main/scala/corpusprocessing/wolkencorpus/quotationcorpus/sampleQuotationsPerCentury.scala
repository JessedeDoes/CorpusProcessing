package corpusprocessing.wolkencorpus.quotationcorpus

import corpusprocessing.wolkencorpus.quotationcorpus.quotationCorpus.getId
import utils.PostProcessXML

import java.io.File
import scala.util.Random
import scala.xml.{Elem, Node, Null, PrefixedAttribute, XML}

case class sampleQuotationsPerCentury(fromDir: String =  addHilexPos.enriched) {


  val toDir = fromDir.replaceAll("/[^/]*/?$", "/CenturySelections")

  val maxFiles = Integer.MAX_VALUE
  def allQuotations() = new File(fromDir).listFiles.iterator.take(maxFiles).flatMap(x => XML.loadFile(x) \\ "cit")

  lazy val quotationDates: Map[String, (Double, Int)] = allQuotations().map(q => {
    val id: String = getId(q)
    val (f,t) = (q \ "date").map(d => (d \ "@atLeast").text -> (d \ "@atMost").text).map({case (f,t) => (f.toInt, t.toInt)}).head
    val average = (f + t) / 2.0
    val nWords = (q \\ "w").size
    id -> (average,nWords)
  }).toMap

  val wordsPerCentury = Map(19 -> 30000, 18 -> 40000, 17 -> 40000, 16 -> 40000, 15 -> 40000, 14 ->50000)
  val wordsPerDecennium = wordsPerCentury.mapValues( _ / 10)

  lazy val perDecennium: Map[Double, List[(String, (Double, Int))]] = quotationDates.groupBy({case (id,(d,n)) => 10 * Math.floor(d/10)}).mapValues(l => Random.shuffle(l).toList)

  def takeNWords(s : Seq[(String, (Double, Int))], n: Int) = {

    def add(total: Int, opgebouwd: Seq[(String, (Double, Int))], item: (String, (Double, Int))): (Int, Seq[(String, (Double, Int))]) = {
      if (total >= n) total -> opgebouwd else  total + item._2._2 -> (opgebouwd :+ item)
    }

    val enhanced: (Int, Seq[(String, (Double, Int))]) = s.foldLeft(0 -> Seq[(String, (Double, Int))]())({case ((t,l), item) => add(t,l,item)})
    enhanced
  }

  def selectIdsForCentury(c: Int) = {
    val plukjes: Seq[(Double, Set[String])] = perDecennium.filter{ case (d, l) => d >= 100*(c-1) && d < 100* (c)}.toList.sortBy({case (d,l) => d}).map({ case (d, l) =>

      val (wc, l1) = takeNWords(l, wordsPerDecennium(c))
      println(s"$d ($wc)  -->  selecteer ${l1.size} van de  ${l.size} citaten")
      d -> l1.map(_._1).toSet
    })
    plukjes.map(_._2).flatten.toSet
  }

  def fixQID(n: Node)  = {
    val e = n.asInstanceOf[Elem]

    PostProcessXML.updateElement(e, x => x.label=="quote" || x.label=="q", q => {
      val cid = getId(e)

      val qPatched = PostProcessXML.updateElement(q, x => Set("pc","w").contains(x.label), w => {
        val newId = cid + "." + getId(w)
        val wPatched = w.copy(attributes =  w.attributes.filter(_.key != "id").append(new PrefixedAttribute("xml","id", newId, Null)))
        // println(wPatched)
        wPatched
      })
      qPatched.copy(attributes = qPatched.attributes.filter(_.key != "id").append(new PrefixedAttribute("xml", "id", "q_" + cid, Null)))
    })
  }

  /*
     Selecteer eerst een set citaatIds (selectIdsForCentury)
     Haal daarna de geselecteerde citaten eruit
   */
  def selectCentury(c: Int)  = {
    val quotation_ids = selectIdsForCentury(c)
    val selectedQuotations: Iterator[Node] = allQuotations().filter(q => quotation_ids.contains(getId(q))).map(fixQID)
    <TEI>
      <text>
        <body>
          <div>
            {selectedQuotations}
          </div>
        </body>
      </text>
    </TEI>
  }
  lazy val centuries =  List(15,16)
  def main(args: Array[String])  = {
    // perDecennium.foreach({case (d,l) => println(d -> l.take(5))})
    new File(toDir).mkdir()
    centuries.foreach(c => {
      val corpusje = selectCentury(c)
      XML.save(toDir + "/"  + s"quotations_$c.xml", corpusje, enc="UTF-8")
    })
  }
}

object sampleQuotationsPerCenturyWNT extends sampleQuotationsPerCentury();
object sampleQuotationsPerCenturyMNW extends sampleQuotationsPerCentury(fromDir = "/mnt/Projecten/Corpora/Historische_Corpora/Wolkencorpus/GTB/CitatenMNW/CitatenTDN/") {
  override val toDir = "/tmp/" // fromDir.replaceAll("/[^/]*/?$", "/CenturySelections")
  override lazy val centuries =  List(14)
};
