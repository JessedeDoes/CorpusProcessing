package corpusprocessing.wolkencorpus.quotationcorpus

import corpusprocessing.wolkencorpus.postProcessBaBTagging
import corpusprocessing.wolkencorpus.quotationcorpus.quotationCorpus._
import utils.PostProcessXML
import utils.PostProcessXML._

import scala.collection.immutable
import scala.util.{Try, Success}
import scala.xml._


case class Quotation(cit: Node, s: Sense, partition: String = "train") {

  lazy val df: String = ((cit \\ "date") \ "@atLeast").map(_.text).headOption.getOrElse("UNK")
  lazy val dt: String = ((cit \\ "date") \ "@atMost").map(_.text).headOption.getOrElse("UNK")

  implicit def t2o[T](x: Try[T]): Option[T] = x match  { case Success(z) => Some(z) ;  case _ => None }

  lazy val timeSpan: Option[Text] = (cit \\ "date")
    .headOption
    .flatMap(x => Try(quotationChronology.averageDate(x)))
    .map(x => 50 * (x / 50))
    .map(x => Text(s"$x-${x+50}"))

  lazy val id: String = getId(cit)

  def hasOref: Boolean = (cit \\ "oRef").nonEmpty

  def hasItAll: Boolean = if (quotationCorpus.forElexis) s.id != "UNK" && hasOref && df != "UNK" && dt != "UNK" && (words.length >= 10) && Try(dt.toInt - df.toInt).map(_ <= 50).getOrElse(false) &&
    quotationCorpus.selectedTimeSpansforElexis.contains(timeSpan.get.text) else s.id != "UNK" && hasOref && df != "UNK" && dt != "UNK" && (words.length >= 10) && Try(dt.toInt - df.toInt).map(_ <= 50).getOrElse(false)


  def addJoin(w: Elem): Elem = {
    val id = getId(w)
    def filterAttribute(a: MetaData) = if (w.label  == "w") true else !Set("type", "lemma").contains(a.key)
    val w0 = w.copy(attributes = w.attributes.filter(filterAttribute))
    val join = if (id.contains("pre")) Some("right") else if (id.contains("pre")) Some("left") else None
    val w1 = if (join.nonEmpty) w0.copy(attributes = w0.attributes.append(new UnprefixedAttribute("join", join.get, Null))) else w0
    w1
  }

  def addSenseRef(x: Node, sid: String, inOref: Boolean) : Node = {
    if (x.label == "w") {
      val w = x.asInstanceOf[Elem].copy(attributes =  x.attributes.filter(_.key != "xmlns"), scope=TopScope)
      if (inOref || (w \\ "oRef").nonEmpty) {
        w.copy(child = Text(w.text), attributes = w.attributes.append(new UnprefixedAttribute("sense-id", sid, Null)))
      } else w.copy(child = Text(w.text))
    } else if (x.isInstanceOf[Elem]) {
      x.asInstanceOf[Elem].copy(child = x.child.map(addSenseRef(_, sid, inOref || x.label == "oRef")))
    } else x
  }

  lazy val q = (cit \ "q").flatMap(
    q =>
      updateElement5(
        {
          val q1 = postProcessBaBTagging.fixDocje(q.asInstanceOf[Elem], ud=quotationCorpus.forElexis)
          val q2 = addSenseRef(q1, senseId, false)
          q2.asInstanceOf[Elem]
        },
        _.label=="interpGrp",
        x => Seq()))

  lazy val words: NodeSeq = q.flatMap(_.descendant.filter(x => x.label=="w" || x.label == "pc"))
  lazy val qtext: String = q.text.replaceAll("\\s+", " ").trim
  lazy val senseId: String = s.id

  lazy val xml = PostProcessXML.updateElement(<cit xml:id={id} sense-id={senseId} entry-id={s.e.id} lemma={s.e.lemma} partition={partition}>
    <date atLeast={df} atMost={dt} timeSpan={timeSpan}/>
    <q>{words}</q>
  </cit>, x => Set("w", "pc").contains(x.label), addJoin)
}

case class Sense(s: Node, e: Entry) {
  lazy val id = getId(s)
  if (id == "UNK") {
    // Console.err.println(s)
  }
  def quotations = (s \ "cit").filter(x => (x \\ "q" \\ "w").nonEmpty).map(Quotation(_, this))
}

case class Entry(e0: Node) {
  val e: Elem = PostProcessXML.updateElement(e0.asInstanceOf[Elem], _.label == "re", x =>  {<opno_deleted/>})

  lazy val lemma: String = ((e \\ "form").filter(f => (f \ "@type").text == "lemma") \ "orth").headOption.map(_.text).getOrElse("UNK")
  lazy val id: String = getId(e)
  lazy val senses: immutable.Seq[Sense] = ((e \\ "sense")).map(Sense(_,this))
  lazy val quotations: immutable.Seq[Quotation] = senses.flatMap(_.quotations)
  lazy val usableQuotations: immutable.Seq[Quotation] = quotations.filter(_.hasItAll)

  def partition(l: Seq[Quotation], pTest: Double, pDev: Double): Seq[Quotation] = {
    val l1 = scala.util.Random.shuffle(l)
    val tSize = (l.size * pTest).toInt
    val dSize = (l.size * pDev).toInt
    val lTest = l1.take(tSize)
    val lDev = l1.drop(tSize).take(dSize)
    val lTrain = l1.drop(tSize + dSize)
    lTest.map(_.copy(partition="test")) ++ lDev.map(_.copy(partition="dev")) ++ lTrain.map(_.copy(partition="train"))
  }
  def xml = <div entry-id={id} lemma={lemma}>
    {partition(usableQuotations, 0.1, 0.1).map(_.xml)}
  </div>
}

object quotationCorpus {

  val forElexis = false
  val selectedTimeSpansforElexis = Set("1500-1550", "1600-1650", "1700-1750", "1800-1850", "1900-1950")
  val minimumQuotationsPerLemmaForElexis = 15

  def getId(e: Node): String = e.attributes.filter(_.key == "id").headOption.map(_.value.text).getOrElse("UNK")

  val oneFile = "/mnt/Projecten/Corpora/Historische_Corpora/Wolkencorpus/GTB/Tagged/wnt10.xml"

  def interestingEntry(e: Entry): Boolean = if (forElexis) e.usableQuotations.size >= minimumQuotationsPerLemmaForElexis && (e.e \ "sense").size > 1 else true

  def extractQuotations(f: String): Elem = {
    val d = XML.load(f)
    val entries = (d \\ "entry").toStream.map(Entry).filter(interestingEntry)

    <TEI><text><body>{entries.map(_.xml)}</body></text></TEI>
  }

  import java.io.{File, PrintWriter}

  def removeNamespace(x: Node):Node = x match {
    case e:Elem => e.copy(scope=TopScope, child = e.child.map(removeNamespace))
    case o => o
  }

  def prettyPrint(pw: PrintWriter, e: Elem): Unit = {
    val pretty = new scala.xml.PrettyPrinter(Integer.MAX_VALUE, 4)
    pw.write(pretty.format(removeNamespace(e)))
    pw.close()
  }

  def f(x: String) = new File(x)
  val outDir = "/mnt/Projecten/Corpora/Historische_Corpora/Wolkencorpus/GTB/CitatenCorpus"

  def main(args: Array[String]): Unit = {
    utils.ProcessFolder.processFolder(
      f("/mnt/Projecten/Corpora/Historische_Corpora/Wolkencorpus/GTB/Tagged"),
      f(s"/mnt/Projecten/Corpora/Historische_Corpora/Wolkencorpus/GTB/${if (forElexis) "CitatenCorpus" else "CitatenTDN"}"),
      {
        case (inputTEI,outputQuotations) =>
          val x = extractQuotations(inputTEI)
          prettyPrint(new PrintWriter(outputQuotations), x)
      })
  }
}
