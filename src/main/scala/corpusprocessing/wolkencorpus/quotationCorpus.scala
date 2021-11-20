package corpusprocessing.wolkencorpus

import scala.xml._
import quotationCorpus._
import utils.PostProcessXML
import utils.PostProcessXML._
import scala.util.{Try,Success,Failure}


case class Quotation(cit: Node, s: Sense) {

  lazy val df = ((cit \\ "date") \ "@atLeast").map(_.text).headOption.getOrElse("UNK")
  lazy val dt = ((cit \\ "date") \ "@atMost").map(_.text).headOption.getOrElse("UNK")

  def hasOreg = (cit \\ "oRef").nonEmpty
  def hasItAll = hasOreg && df != "UNK" && dt != "UNK" && (words.length >= 10) && Try(dt.toInt - df.toInt).map(_ <= 50).getOrElse(false)

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
          val q1 = postProcessBaBTagging.fixDocje(q.asInstanceOf[Elem])
          val q2 = addSenseRef(q1, senseId, false)
          q2.asInstanceOf[Elem]
        },
        _.label=="interpGrp",
        x => Seq()))

  lazy val words = q.flatMap(_.descendant.filter(x => x.label=="w" || x.label == "pc"))
  lazy val qtext = q.text.replaceAll("\\s+", " ").trim
  lazy val senseId = s.id
  lazy val xml = <cit sense-id={senseId} entry-id={s.e.id} lemma={s.e.lemma}>
    <date atLeast={df} atMost={dt}/>
    <q>{words}</q>
  </cit>
}

case class Sense(s: Node, e: Entry) {
  lazy val id = getId(s)
  if (id == "UNK") {
    // Console.err.println(s)
  }
  def quotations = (s \ "cit").filter(x => (x \\ "q" \\ "w").nonEmpty).map(Quotation(_, this))
}

case class Entry(e0: Node) {
  val e = PostProcessXML.updateElement(e0.asInstanceOf[Elem], _.label == "re", x =>  {
    //Console.err.println("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!Deleting "  + x)
    // System.exit(1)
      <opno_deleted/>})

  lazy val lemma = ((e \\ "form").filter(f => (f \ "@type").text == "lemma") \ "orth").headOption.map(_.text).getOrElse("UNK")
  lazy val id = getId(e)
  lazy val senses = ((e \\ "sense")).map(Sense(_,this))
  lazy val quotations = senses.flatMap(_.quotations)
  lazy val usableQuotations = quotations.filter(_.hasItAll)
  def xml = <div entry-id={id} lemma={lemma}>
    {usableQuotations.map(_.xml)}
  </div>
}

object quotationCorpus {
  def getId(e: Node) = e.attributes.filter(_.key == "id").headOption.map(_.value.text).getOrElse("UNK")
  val oneFile = "/mnt/Projecten/Corpora/Historische_Corpora/Wolkencorpus/GTB/Tagged/wnt10.xml"

  def doFile(f: String) = {
    val d = XML.load(f)
    val entries = (d \\ "entry").toStream.map(Entry).filter(x => x.usableQuotations.size >= 10 && (x.e \ "sense").size > 1)
    <TEI><text><body>{entries.map(_.xml)}</body></text></TEI>
  }

  import java.io.File
  import java.io.PrintWriter
  def pretty(pw: PrintWriter, e: Elem)  = {
    val pretty = new scala.xml.PrettyPrinter(Integer.MAX_VALUE, 4)
    pw.write(pretty.format(e))
    pw.close()
  }


  def f(x: String) = new File(x)
  def main(args: Array[String]): Unit = {
    utils.ProcessFolder.processFolder(f("/mnt/Projecten/Corpora/Historische_Corpora/Wolkencorpus/GTB/Tagged"), f("/mnt/Projecten/Corpora/Historische_Corpora/Wolkencorpus/GTB/CitatenCorpus"),
      {
        case (i,o) => val x = doFile(i)
          pretty(new PrintWriter(o), x)
      })
  }
}
