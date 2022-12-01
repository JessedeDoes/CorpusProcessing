package corpusprocessing.wolkencorpus.quotationcorpus

import corpusprocessing.wolkencorpus.postProcessBaBTagging
import corpusprocessing.wolkencorpus.quotationcorpus.quotationCorpus._
import database.DatabaseUtilities.{AlmostQuery, ResultMapping, Select}
import database.{Configuration, Database}
import utils.{PostProcessXML, ProcessFolder}
import utils.PostProcessXML._

import java.io.File
import scala.collection.immutable
import scala.util.{Random, Success, Try}
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

  lazy val pos = ((e \ "grampGrp") ++ (e \ "dictScrap" \ "gramGrp")).flatMap(x => (x \ "pos").map(_.text)).mkString(" ")

  def partition(l: Seq[Quotation], pTest: Double, pDev: Double): Seq[Quotation] = {
    val l1 = scala.util.Random.shuffle(l)
    val tSize = (l.size * pTest).toInt
    val dSize = (l.size * pDev).toInt
    val lTest = l1.take(tSize)
    val lDev = l1.drop(tSize).take(dSize)
    val lTrain = l1.drop(tSize + dSize)
    lTest.map(_.copy(partition="test")) ++ lDev.map(_.copy(partition="dev")) ++ lTrain.map(_.copy(partition="train"))
  }
  def xml = <div entry-id={id} lemma={lemma} pos={pos}>
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
      f(s"/mnt/Projecten/Corpora/Historische_Corpora/Wolkencorpus/GTB/${if (forElexis) "CitatenCorpus" else "CitatenTDN2"}"),
      {
        case (inputTEI,outputQuotations) =>
          val x = extractQuotations(inputTEI)
          prettyPrint(new PrintWriter(outputQuotations), x)
      })
  }
}
case class Omspelling(id: Int, modern_lemma: String, historical_lemma: String, spelling_id: String) {
  val dictionary = spelling_id.replaceAll("^(MNW|WNT|VMNW|ONW).*","$1")
  val persistent_id = spelling_id.replaceAll("^(MNW|WNT|VMNW|ONW)","")
  def hoofdLemma = dictionary == "MNW"  || persistent_id.matches("[A-Z][0-9]+")
  def lemma_id = spelling_id.replaceAll(dictionary,"")
}

object HilexInfo
{
  val hilexCandidate = Configuration(
    name = "gigant_hilex_candidate",
    server = "svowdb16.ivdnt.loc",
    database = "gigant_hilex_candidate",
    user = "dba",
    password = "vercingetorix")
  val getOmspelling = ResultMapping[Omspelling](r => Omspelling(r.getInt("id"), r.getString("modern_lemma"), r.getString("hist_lemma"), r.getString("hist_lemma_id")))
  val getPosQuery = Select(r => r.getString("persistent_id") -> r.getString("lemma_part_of_speech"), "data.lemmata")
  lazy val hilex: Database = new database.Database(hilexCandidate)

  def alleOmspellingen: AlmostQuery[Omspelling] = {
    db =>
      db.createQuery(
        s"""
      select * from
           marijke_spelling.spelling
          """).map(getOmspelling)
  }

  def main(Args: Array[String]) = {
    hilex.slurp(getPosQuery).foreach(println)
  }
}

// Hilex PoS toevoegen waar mogelijk
object addHilexPos {
  lazy val hilex: Database = new database.Database(HilexInfo.hilexCandidate)
  lazy val alleOmspellingen: Map[String, String] = hilex.slurp(HilexInfo.alleOmspellingen).map(o => o.lemma_id -> o.modern_lemma).toMap
  lazy val alleHilexPos: Map[String, String] = hilex.slurp(HilexInfo.getPosQuery).toMap

  def addHilexInfo(cit: Elem)  = {
    val id: String = getId(cit)
    val lemmaId = id.replaceAll("\\.eg.*","")
    val hilexPos = alleHilexPos.getOrElse(lemmaId,"unk")
    val omspelling = alleOmspellingen.getOrElse(lemmaId,"unk")
    // System.err.println(s"$lemmaId $omspelling $hilexPos")
    val atts = cit.attributes.append(new UnprefixedAttribute("pos",hilexPos,Null)).append(new UnprefixedAttribute("modern-lemma",omspelling,Null))
    val c = cit.copy(attributes = atts)
    //System.err.println(c.copy(child=Seq()))
    c
  }

  def fixDoc(d: Elem)  = PostProcessXML.updateElement(d, _.label=="cit", addHilexInfo)

  implicit def f(s: String) : java.io.File = new java.io.File(s)

  val base = "/mnt/Projecten/Corpora/Historische_Corpora/Wolkencorpus/GTB/CitatenTDN2/"
  val lemmatized = base + "Lemmatized/"
  val enriched = base + "Refurbished/"
  def main(args: Array[String]) = {
    ProcessFolder.processFolder(lemmatized, enriched, {case (i,o) =>
       System.err.println("File:"  + i)
       val d = XML.load(i)
       val d1 = fixDoc(d)
       XML.save(o,d1,"UTF-8")
    }, parallel = false)
  }
}


object sampleQuotationsPerCentury {

  val fromDir  = addHilexPos.enriched
  val toDir = fromDir.replaceAll("/[^/]*/?$", "/CenturySelections")
  new File(toDir).mkdir()

  def allQuotations() = new File(fromDir).listFiles.iterator.take(60).flatMap(x => XML.loadFile(x) \\ "cit")

  lazy val quotationDates: Map[String, (Double, Int)] = allQuotations().map(q => {
    val id: String = getId(q)
    val (f,t) = (q \ "date").map(d => (d \ "@atLeast").text -> (d \ "@atMost").text).map({case (f,t) => (f.toInt, t.toInt)}).head
    val average = (f + t) / 2.0
    val nWords = (q \\ "w").size
    id -> (average,nWords)
  }).toMap

  val wordsPerCentury = Map(19 -> 30000, 18 -> 40000, 17 -> 40000)
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

  def selectCentury(c: Int)  = {
    val quotation_ids = selectIdsForCentury(c)
    val selectedQuotations = allQuotations().filter(q => quotation_ids.contains(getId(q)))
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

  def main(args: Array[String])  = {
    // perDecennium.foreach({case (d,l) => println(d -> l.take(5))})
     List(17,18,19).foreach(c => {
       val corpusje = selectCentury(c)
        XML.save(toDir + "/"  + s"quotations_$c.xml", corpusje, enc="UTF-8")
     })
  }
}

/*
            <div lemma="AANZWOEGEN" entry-id="M001105">
                <cit modern-lemma="aanzwoegen" pos="VRB" partition="train" lemma="AANZWOEGEN" entry-id="M001105" sense-id="M001105.bet.1" xml:id="M001105.eg.5510">
                    <date timeSpan="1850-1900" atMost="1866" atLeast="1866"/>
                    <q xml:id="no_doc_id.s.0">
                        <w lexicon="molex" xml:id="e2202b70-1c8b-44b0-9272-53fb847ea519.w.63" pos="ADV(type=reg)" lemma="daar">Daar</w>
                        <w lexicon="molex" xml:id="e2202b70-1c8b-44b0-9272-53fb847ea519.w.64" pos="VRB(finiteness=fin,tense=pres)" lemma="komen">komt</w>
                        <w lexicon="molex" xml:id="e2202b70-1c8b-44b0-9272-53fb847ea519.w.65" pos="PD(type=d-p,subtype=art,position=prenom)" lemma="de">de</w>
                        <w lexicon="molex" xml:id="e2202b70-1c8b-44b0-9272-53fb847ea519.w.66" pos="NOU-C(number=sg)" lemma="kruier">kruier</w>
                        <w lexicon="molex" xml:id="e2202b70-1c8b-44b0-9272-53fb847ea519.w.67" pos="ADV(type=reg)" lemma="al">al</w>
                        <w lexicon="molex" xml:id="e2202b70-1c8b-44b0-9272-53fb847ea519.w.68" pos="ADP(type=pre)" lemma="met">met</w>
                        <w lexicon="hilex" xml:id="e2202b70-1c8b-44b0-9272-53fb847ea519.w.69" pos="PD(type=poss,position=prenom)" lemma="u">uw</w>
                        <w lexicon="molex" xml:id="e2202b70-1c8b-44b0-9272-53fb847ea519.w.70" pos="NOU-C(number=pl)" lemma="koffer">koffers</w>
                        <w lexicon="hilex" xml:id="e2202b70-1c8b-44b0-9272-53fb847ea519.w.71" pos="VRB(finiteness=inf)" lemma="aanzwoegen" sense-id="M001105.bet.1">aanzwoegen</
w>
                        <pc pos="LET" xml:id="e2202b70-1c8b-44b0-9272-53fb847ea519.pc.pre.0000020" join="right">(</pc>
                        <w lexicon="molex" xml:id="e2202b70-1c8b-44b0-9272-53fb847ea519.w.72" pos="CONJ(type=coor)" lemma="of">of</w>
                        <pc pos="LET" xml:id="e2202b70-1c8b-44b0-9272-53fb847ea519.pc.floating.0000021">—</pc>
                        <w lexicon="hilex" xml:id="e2202b70-1c8b-44b0-9272-53fb847ea519.w.74" pos="VRB(finiteness=pastpart)" lemma="aanzwoegen" sense-id="M001105.bet.1">aangezw
oegd</w>
                        <pc pos="LET" xml:id="e2202b70-1c8b-44b0-9272-53fb847ea519.pc.post.0000022">).</pc>
                    </q>
                </cit>
            </div>

 */

/*
<TEI>
    <text>
        <body>
            <div entry-id="M064643" lemma="SNAKKER"> </div>
            <div entry-id="M064644" lemma="SNAP">
                <cit xml:id="M064644.eg.37073" sense-id="M064644.sense.7" entry-id="M064644" lemma="SNAP" partition="test">
                    <date atLeast="1657" atMost="1657" timeSpan="1650-1700"/>
                    <q xml:id="no_doc_id.s.0">
                        <w lemma="wie" pos="PD(type=w-p,position=free)" xml:id="a7ddb939-9191-4609-92c8-e19c2341dbf7.w.399" lexicon="molex">Wiens</w>
                        <pc xml:id="a7ddb939-9191-4609-92c8-e19c2341dbf7.pc.floating.00000139" pos="AA(degree=pos,position=prenom)" lemma="de⊕een" lexicon="byt5">…</pc>
                        <w lemma="vrouw" pos="NOU-C(number=pl)" xml:id="a7ddb939-9191-4609-92c8-e19c2341dbf7.w.401" lexicon="molex">vrouwen</w>
                        <w lemma="en" pos="CONJ(type=coor)" xml:id="a7ddb939-9191-4609-92c8-e19c2341dbf7.w.402" lexicon="hilex">ende</w>
                        <w lemma="kind" pos="NOU-C(number=pl)" xml:id="a7ddb939-9191-4609-92c8-e19c2341dbf7.w.403" lexicon="molex">kinderen</w>
                        <w lemma="met" pos="ADP(type=pre)" xml:id="a7ddb939-9191-4609-92c8-e19c2341dbf7.w.404" lexicon="molex">met</w>

 */

