package corpusprocessing.CLVN

import corpusprocessing.CLVN.PostProcessForCobaltje.{fix, p, p1, process, putAllInSeg}
import corpusprocessing.metadata.jamaarCLVN

import scala.xml.{Elem, NodeSeq, _}
import utils.PostProcessXML._
import utils.ProcessFolder

import java.io.File

object CLVNUtils {

  def cleanRefs(d:Elem) = updateElement(d, _.label=="xref", xrefToRep)
  def cleanHi(d: Elem):Elem = updateElement5(d, _.label=="hi", noEmptyHi).asInstanceOf[Elem]

  def cleanPB(p: Elem): NodeSeq = {
    val unit = (p \ "@unit").toString()
    if (unit.equals("dollar")) Seq() else p.copy(attributes = p.attributes.filter(a => a.key != "unit"))
  }
  def validCSS = Set("font-weight", "font-size", "font-style", "display", "text-decoration", "vertical-align")

  def ditPToPB(p: Elem): Elem = {
    if (p.child.size == 1 && p.child.head.isInstanceOf[Text]) {
      val txt = p.child.head.toString.trim
      if (txt.matches("^p\\s*[0-9]+\\s*$")) {
        val n = txt.replaceAll("\\s*p\\s*", "").trim
        // Console.err.println(s"Yep!:$p => $n")
        return <pb n={n}/>
      }
    }
    if (p.child.filter(_.label != "lb").map(_.label).toList == List("hi")) {
      val hi = p \ "hi"
      if ((hi \ "@rend").toString == "font-weight:bold") {
        val hic = hi.head.asInstanceOf[Elem].child
        if (hic.size == 1 && hic.head.isInstanceOf[Text] && hic.head.toString.trim.matches("^p\\s*[0-9]+\\s*$")) {
          val n = hic.head.toString.replaceAll("\\s*p\\s*", "").trim
          // Console.err.println(s"Yep!:$p => $n")
          return <pb n={n}/>
        }
      }
    }
    p
  }

  def ditPeeBees(d: Elem): Elem = updateElement(d, _.label == "p", ditPToPB)

  def cleanRendition(r: String): Option[String] = {
    if (r.isEmpty)
      None
    else {
      val props = r.trim.split("\\s*;\\s*").map(fv => fv.split("\\s*:\\s*")).map(x => x(0) -> x(1))
      val keepProps = props.filter(f => validCSS.contains(f._1) && !(f._2 == "inline")).map(x => s"${x._1}:${x._2}").mkString(";")
      // Console.err.println(s"${props.toList} --> $keepProps")
      if (keepProps.isEmpty) None
      else Some(keepProps)
    }
  }

  def noEmptyHi(p: Elem): NodeSeq = {
    val txt = p.text.toString.replaceAll("\\p{Z}", "").trim
    val rend = cleanRendition((p \\ "@rend").text)
    val hasIt = "\\S".r.findFirstIn(txt)

    if (txt.isEmpty) Seq()
    else if (rend.isEmpty)
      p.child
    else {
      val newRend = new UnprefixedAttribute("rend", rend.get, Null)
      Seq(p.copy(attributes = p.attributes.filter(a => a.key != "type" && a.key != "rend").append(newRend)))
    }
  }



  def xrefToRep(x: Elem): Elem = {
    val link: Option[String] = x.attributes.find(_.key == "url").map(_.value.text)
    val newAtts = x.attributes.filter(_.key != "url").append(new UnprefixedAttribute("target", link.getOrElse("#unknown"), Null))
    x.copy(label = "ref", attributes = newAtts)
  }

  def getId(n: Node): Option[String] = n.attributes.filter(a => a.prefixedKey.endsWith(":id") || a.key == "id").map(_.value.toString).headOption

  def flattenMilestone(e: Elem): NodeSeq = {
    val id = getId(e).get.toString
    Seq(<milestone xml:id={id + "-b"}/>) ++ e.child ++ Seq(<milestone xml:id={id + "-e"}/>)
  }

  def flattenMilestones(d: Elem): Elem = updateElement2(d, e => e.label == "milestone" && e.child.nonEmpty, flattenMilestone).head.asInstanceOf[Elem]

  def renameAttribute(e: Elem, from: String, to: String) =
    e.attributes.get(from).map(_.toString).map(new UnprefixedAttribute(to, _, Null))

  def toMetadata(l: List[UnprefixedAttribute]): MetaData = {
    if (l.isEmpty) Null else
      l.tail.foldLeft(l.head.asInstanceOf[MetaData])({ case (m, a) => m.append(a) })
  }


  def updateGap(e: Elem): Elem = {
    val keys = e.attributes.map(_.key.toString)
    val extendt = renameAttribute(e, "extend", "extent")
    val extent = renameAttribute(e, "extent", "extent")
    val reason = renameAttribute(e, "reason", "reason")
    val resp = renameAttribute(e, "resp", "resp")

    val atts = List(extendt, extent, reason, resp).filter(_.isDefined).map(_.get)

    e.copy(attributes = toMetadata(atts))
  }

  def updateGaps(d: Elem): Elem = updateElement(d, e => e.label == "gap", updateGap)

  def margeToAdd(e: Elem): Elem = {
    val place = e.attribute("place")
    val pos = e.attribute("pos")
    val attributes = if (place.isEmpty) e.attributes.append(new UnprefixedAttribute("place", "margin", Null)) else e.attributes
    e.copy(label = "add", attributes = attributes)
  }


  def addAna(inst2bibl: Map[String, Set[String]])(e: Elem): Elem = {
    val id = getId(e)
    if (id.isDefined) {
      val anas: Option[Set[String]] = inst2bibl.get(id.get)
      if (anas.isDefined) {
        val ana = new UnprefixedAttribute("ana", anas.get.map(x => s"#$x").mkString(" "), Null)
        e.copy(attributes = e.attributes.append(ana))
      } else e
    } else e
  }

  def addAnas(d: Elem): Elem = {
    val list = (d \\ "bibl").flatMap(
      b => {

        val id = getId(b)
        if (id.isDefined) {
          val spans = b \\ "span"

          spans.flatMap(s => {
            val i0: Option[String] = (s \ "@inst").headOption.map(_.toString()).map(_.substring(1))
            val f = (s \ "@from").headOption.map(_.toString()).map(_.substring(1))
            val t = (s \ "@to").headOption.map(_.toString()).map(_.substring(1))
            if (i0.isDefined) {
              List(i0.get -> id.get)
            } else if (f.isDefined) {
              List(f.get -> id.get, t.get -> id.get)
            } else List()
          })
        } else List()
      })

    val inst2bibl: Map[String, Set[String]] = list.groupBy(_._1).mapValues(_.toSet).mapValues(_.map(_._2))
    updateElement3(d, e => true, addAna(inst2bibl))
  }


  def renameMargeToAdd(d: Elem): Elem = updateElement(d, e => List("marge", "margin", "marginextra").contains(e.label.toLowerCase()), margeToAdd)

  private def ontWiebelElem(e: Elem): Seq[Node] = Seq(Text("[")) ++ e.child ++ Seq(Text("]"))

  def ontWiebel(d: Elem): Elem = updateElement2(d, e => e.label == "wiebel", ontWiebelElem).asInstanceOf[Elem]

  private def renameSubtext(e: Elem) = {
    val id = getId(e)
    if (id.isEmpty) {
      Console.err.println(s"!!!Geen id op subtekst in ${e.toString}")
    }
    <floatingText xml:id={getId(e).getOrElse("id_missing_on_subtext")}>
      <body>
        <div>
          {e.child}
        </div>
      </body>
    </floatingText>
  }

  def subTextToTEI(d: Elem): Elem = updateElement(d, x => x.label == "subtekst" || x.label == "floating_text", renameSubtext)

  def optionToList[T](x: Option[T]) = x match {
    case Some(y) => List(y);
    case None => List()
  }

  def hasLabel(n: Node, l: String): Boolean = n.label.toString.equalsIgnoreCase(l)

  def makeDivLevels(l: Seq[Node], kopjes: List[String]): Seq[Node] = {
    if (kopjes.isEmpty) l
    else {
      val groups = groupWithFirst(l, hasLabel(_, kopjes.head))
      //groups.foreach(g =>  println(s"at ${kopjes.head} $g"))
      val newChild = groups.map(g => {
        val head = optionToList(g.find(hasLabel(_, kopjes.head))) // neen ...
        val tail = g.dropWhile(hasLabel(_, kopjes.head))
        val tailProcessed = if (kopjes.tail.nonEmpty && tail.exists(hasLabel(_, kopjes.tail.head)))
          makeDivLevels(tail, kopjes.tail)
        else tail // dit alleen als er aanleiding toe bestaat


        // soms is er niks. geef dan ook niks terug?
        <div type={kopjes.head}>
          {head.map(h => <head>
          {h.child}
        </head>)}{tailProcessed}

        </div>
      })
      newChild
    }
  }

  def divStructureFromHeadElements(e: Elem, kopjes: List[String]): Elem = {
    e.copy(child = makeDivLevels(e.child, kopjes))
  }

  def createDivStructure(e: Elem): Elem = {
    val koppies = e \ "h1" ++ e \ "H1"
    if ((e \ "div").nonEmpty) // dit is eng
      e
    else if (koppies.nonEmpty)
      divStructureFromHeadElements(e, List("h1", "h2", "h3"))
    else
      e.copy(child = Seq(<div>
        <p>
          {e.child}
        </p>
      </div>))
  }

  def createBasicDocumentStructure(d: Elem): Elem = {
    val backjes = (d \\ "back").toSeq.map(_.asInstanceOf[Elem]).map(createDivStructure)
    val frontjes = (d \\ "front").toSeq.map(_.asInstanceOf[Elem]).map(createDivStructure)

    val body = (if ((d \ "body").isEmpty)
      <body>
        {d.child.filter(n => !(n.label == "back" || n.label == "front"))}
      </body>
    else (d \ "body")).asInstanceOf[Elem]

    d.copy(child = frontjes ++ createDivStructure(body) ++ backjes)
  }

  def magInDivje(n: Node): Boolean = n.label == "div" || n.label == "p" || n.label == "pee" || n.label == "head" || n.text == ""

  def pakInInPeetjes(l: Seq[Node]): Seq[Node] = {
    val groups = groupWithFirst(l, magInDivje)
    groups.flatMap(g => {
      val head = optionToList(g.find(magInDivje))
      val tail = g.dropWhile(magInDivje)
      head ++ (if (tail.nonEmpty) Seq(<p type="inserted">
        {tail}
      </p>) else tail)
    }
    )
  }

  def PeetjesInDivjes(d: Elem): Elem = updateElement3(d, _.label == "div", e => e.copy(child = pakInInPeetjes(e.child)))

  def supToHi(e: Elem): Elem = {
    val rend = e.label.toLowerCase() match {
      case "sup" => "vertical-align:super"
      case "u" => "text-decoration:underline"
    }
    <hi rend={rend}>
      {e.child}
    </hi>
  }

  def supsToHi(d: Elem): Elem = updateElement3(d, x => List("u", "sup").contains(x.label.toLowerCase), supToHi)

  def noEmptyP(p: Elem): NodeSeq = {
    val txt = p.text.toString.replaceAll("\\p{Z}", "").trim
    val hasIt = "\\S".r.findFirstIn(txt)

    if (txt.isEmpty) Seq() else Seq(p.copy(attributes = p.attributes.filter(_.key != "type")))
  }

  def optionOr[A](a: Option[A], b: Option[A]) = if (a.isDefined) a else b

  def fixId(b: Boolean)(e: Elem): Elem = {
    val id: Option[String] = optionOr(
      (e \ "@id").map(_.toString()).headOption,
      e.attributes.filter(_.prefixedKey == "xml:id").map(_.value.text).headOption)

    if (id.nonEmpty) {
      val xmlId = new UnprefixedAttribute("xml:id", (if (b) "INT_" else "") + id.get, Null)
      e.copy(attributes = e.attributes.filter(a => a.key != "id" && a.prefixedKey != "xml:id").append(xmlId))
    } else e
  }

  def fixIds0(d: Elem): Elem = updateElement3(d, e => true, fixId(false))

  def fixIds1(d: Elem): Elem = {

    updateElement3(d, e => true, fixId(true))
  }



  def cleanPBs(d: Elem): Elem = updateElement5(d, _.label == "pb", cleanPB).asInstanceOf[Elem]

  def cleanP(d: Elem): Elem = updateElement5(d, _.label == "p", noEmptyP).asInstanceOf[Elem]

  def addUnitToMilestone(m: Elem) = {
    val id = getId(m)
    if (id.isDefined) {
      val unit = if (id.get.toString.endsWith("b")) {
        "span_start"
      } else {
        "span_end"
      }
      m.copy(attributes = m.attributes.append(new UnprefixedAttribute("unit", unit, Null)))
    } else m
  }

  def fixMilestones(d: Elem) = updateElement(d, _.label == "milestone", addUnitToMilestone)

  def posIsPlace(d: Elem) = updateElement(d,
    e => e.label == "add" && (e \ "@pos").nonEmpty,
    e => e.copy(attributes = e.attributes.filter(_.key != "pos").append(new UnprefixedAttribute("place", (e \ "@pos").toString, Null)))
  )

  def addFloatingText(q: Elem) = {
    val id = getId(q)
    if (id.isDefined && (q \ "floatingText").isEmpty) {
      <q>
        <floatingText xml:id={id.get}>
          <body>
            <div>
              <p>
                {q.child}
              </p>
            </div>
          </body>
        </floatingText>
      </q>
    } else q
  }

  def floatingTextInQ(d: Elem): Elem = updateElement3(d, _.label == "q", addFloatingText)

  def deTokenizeToken(w: Elem)  = {
    if ((w \ "seg").nonEmpty && w.label != "s") (w \ "seg").flatMap(_.child) else w.child
  }

  def deTokenizeDoc(d: Elem) =
    updateElement5(d, x => Set("w", "pc", "s").contains(x.label), deTokenizeToken).asInstanceOf[Elem]
}


object deTokenize {

  val in = new File(Settings.CLNVPatched)
  val out = new File(Settings.CLNVDetokenized)
  def process(in: String, out: String)  = {

    XML.save(out, CLVNUtils.deTokenizeDoc(XML.load(in)))
  }
  def main(args: Array[String]) = {

    ProcessFolder.processFolder(in,out,process)
    p.close()
    p1.close()
  }
}
