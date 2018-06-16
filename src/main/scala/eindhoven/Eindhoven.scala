package eindhoven

import java.io.{File, FileWriter}
import java.text.Normalizer

import eindhoven.Eindhoven.{geslachtenMap, replaceFeature, _}
import posmapping.ProcessFolder

import scala.util.{Try,Success,Failure}
import scala.xml._

case class Word(word: String, lemma: String, pos: String) {
  def matches(mp: Int, sp: Int, ssp: Int): Boolean = {

    mp match {
      case 0 => if (List(0, 2, 8, 9).contains(sp)) pos.startsWith("NOU-C") &&
        !(ssp == 0 && !pos.contains("sg")) && !(ssp == 1 && !pos.contains("pl"))
      else pos.startsWith("NOU-P")
      case 1 => pos.startsWith("AA")
      case 2 => pos.startsWith("VRB") &&
        !(sp >= 4 && (ssp == 5 || ssp == 6) && !pos.contains("past")) &&
        !(sp >= 4 && List(1, 2, 3, 4).contains(ssp) && !pos.contains("pres")) &&
        !(List(0, 1, 2, 3).contains(sp) && !(pos.contains("=inf") || pos.contains("part")))
      case 3 => pos.startsWith("PD") &&
        !(List(2, 3).contains(sp) && !pos.contains("poss")) &&
        !(List(4, 5).contains(sp) && !pos.contains("ref")) &&
        !(List(0).contains(sp) && !pos.contains("per"))
      case 4 => (pos.startsWith("PD") || pos.startsWith("NUM")) && !(pos.contains("poss"))
      case 5 => pos.startsWith("ADV")
      case 6 => pos.startsWith("ADP")
      case 7 => pos.startsWith("CONJ")
      case 8 => pos.startsWith("INT")
      case _ => true
    }
  }
}

object Eindhoven {

  import scala.io.Source._


  val atHome = true


  val baseDirAtWork = "/mnt/Projecten/Nederlab/Tagging/TKV_Eindhoven/"
  val baseDirAtHome = "/mnt/DiskStation/homes/jesse/work/Eindhoven/TKV_Eindhoven/"

  val baseDir = if (atHome) baseDirAtHome else baseDirAtWork

  lazy val lemmaLog = new FileWriter(baseDir + "lemma.log")

  def noAccents(s: String): String = Normalizer.normalize(s, Normalizer.Form.NFD).replaceAll("\\p{M}", "").toLowerCase.trim


  val hulpDataDir = baseDir + "hulpdata/"
  val xmlDir = baseDir + "xml-with-word-ids/"
  val patchDir = baseDir + "xml-tagged/"
  val outputDir = baseDir + "tkvPatch/"

  case class Tag(tag: String) {
    val pos = tag.replaceAll("\\(.*", "")
    val features = tag.replaceAll(".*\\(", "").replaceAll("\\).*", "").split(",").toList
    val cgnFeatures = features.filter(!_.matches("^[e](x?)-.*"))
    val extFeatures = features.filter(_.matches("^[e](x?)-.*"))

    override def toString = s"$pos(${features.mkString(",")})"
  }

  val cgnTags = fromFile("data/cgn.tagset").getLines.toSet.map(Tag(_))


  import sparql2xquery.TopologicalSort._

  def featureOrder(pos: Tag => Boolean):Option[Map[String,Int]] =
  {
    val twp = cgnTags.filter(pos).toList

    val relations:List[(String,String)] = twp.flatMap(
      tag => {
        val fi = tag.features.zipWithIndex
        fi.flatMap({ case (f, i) => fi.filter(_._2 == i+1).map(f1 => f ->  f1._1) })
      }
    )

    scala.util.Try(tsort(relations)) match
      {
      case Failure(e) => None
      case scala.util.Success(x) =>  {
        Some(x.toList.zipWithIndex.toMap)
      }
    }
  }

  cgnTags.map(_.pos).foreach(p => featureOrder(t => t.pos == p))

  val allPronTypes = cgnTags.filter(_.pos == "VNW").map(_.features(0))

  allPronTypes.foreach(
    pdtype => {
      Console.err.println(s"Hola:$pdtype")
      featureOrder(t => t.pos == "VNW" && t.features.contains(pdtype))
    }
  )


  val mainFeatureOrderMap = cgnTags.map(_.pos).filter(_ != "VNW").map(p => p -> featureOrder(_.pos == p).get).toMap
  def pronWithType(ptype:String)(t: Tag) = t.pos == "VNW" && t.features(0) == ptype

  val vnwFeatureOrderMap = allPronTypes.map(ptype => (ptype -> featureOrder(pronWithType(ptype)).get)).toMap

  def orderFeatures(t: Tag): String = {

    Try({
      val order = if (t.pos == "VNW") vnwFeatureOrderMap(t.features(0)) else mainFeatureOrderMap(t.pos)
      def ordering(x: String) : Int = order( x.replaceAll("\\|.*", "").replaceAll("x-", ""))
      val fs = Try(t.cgnFeatures.sortBy(ordering)) match {
        case Success(x) => {}
        case Failure(e) => Console.err.println(s"Cannot sort $t"); return t.toString;
      }
      val allFeaturesSorted = t.cgnFeatures.sortBy(ordering) ++ t.extFeatures
      s"${t.pos}(${allFeaturesSorted.mkString(",")})"
    }) match {
      case Success(s) => s
      case Failure(e) => t.toString
    }
  }

  def orderFeatures(t: String):String = orderFeatures(Tag(t))

  val verkleinvormen = fromFile("data/verkleinwoordvormen.txt").getLines.toSet

  val geslachtenMap = fromFile("data/geslacht.txt").getLines.toList.map(l => l.split("\\t")).map(r => r(0) -> r(1)).toMap


  val namenMap = scala.io.Source.fromFile("data/namenKlus.txt").getLines.toStream.map(l => l.split("\\s*->\\s*")).filter(_.size > 1).map(l => l(0).trim -> l(1).trim).toMap

  val files = new java.io.File(xmlDir).listFiles.toStream.flatMap(f => f.listFiles().toList).filter(f => f.getName().endsWith(("xml")))

  val goodies = scala.io.Source.fromFile(hulpDataDir + "/" + "goede-woorden-uit-molex-postgres.csv").getLines.toList.map(l => l.split(";"))
    .map(r => r.map(_.trim.replaceAll(""""""", ""))).map(r => Word(r(1), r(0), r(2))
  )

  val deelwoorden = goodies.filter(w => w.pos.contains("=part") && !w.word.endsWith("n"))
  val extraDeelwoorden = deelwoorden.map(w => w.copy(word = w.word + "e"))


  val goodiesPlus = goodies ++ extraDeelwoorden

  val tagMapping = io.Source.fromFile("data/vu.taginfo.csv").getLines().map(l => l.split("\\s+")).filter(_.size >= 2)
    .map(l => l(0) -> l(1).replaceAll("\\s.*", "")).toMap

  def mapTag(t: String) =
    {
      val t0 = t.substring(0,Math.min(3,t.length))
      tagMapping.getOrElse(t0, s"UNDEF($t)")
    }

  // ‹vu 000›‹hvh-kort N(soort,ev,neut) ›‹hvh-lang N(com,numgen=singn,case=unm,Psynuse=nom)›
  val hvh_kort = "‹hvh-kort\\s*(.*?)\\s*›".r
  val hvh_lang = "‹hvh-lang\\s*(.*?)\\s*›".r
  val vu_pos = "‹vu\\s*(.*?)\\s*›".r

  def hvhKort(e: Node): Option[String] = (e \\ "@pos").flatMap(a =>
    hvh_kort.findFirstMatchIn(a.text).map(_.group(1))
  ).headOption.map(x => x.replaceAll(":.*", ""))

  def hvhLang(e: Node): Option[String] = (e \\ "@pos").flatMap(a =>
    hvh_lang.findFirstMatchIn(a.text).map(_.group(1))
  ).headOption


  def vuPos(e: Node): Option[String] = (e \\ "@pos").flatMap(a =>
    vu_pos.findFirstMatchIn(a.text).map(_.group(1))
  ).headOption


  val goodMap: Map[String, List[Word]] = goodiesPlus.groupBy(w => noAccents(w.word))


  def updateElement(e: Elem, condition: Elem => Boolean, f: Elem => Elem): Elem = {
    if (condition(e))
      f(e)
    else
      e.copy(child = e.child.map({
        {
          case e1: Elem => updateElement(e1, condition, f)
          case n: Node => n
        }
      }))
  }

  def updateElement5(e: Elem, condition: Elem => Boolean, f: Elem => NodeSeq): NodeSeq = {
    val newChildren = e.child.flatMap({
      {
        case e1: Elem => updateElement5(e1, condition, f)
        case n: Node => Seq(n)
      }
    })

    if (condition(e)) {
      f(e.copy(child = newChildren))
    }
    else
      e.copy(child = newChildren)
  }


  def capFirst(s: String) = s.substring(0, 1).toUpperCase + s.substring(1, s.length)

  def vuPatchS(d: Elem): Elem = {
    val numberedChildren = d.child.zipWithIndex
    val firstWordIndex: Int = numberedChildren.find(_._1.label == "w").get._2
    val firstWord: Elem = numberedChildren(firstWordIndex)._1.asInstanceOf[Elem]
    val newFirstWord = firstWord.copy(child = Text(capFirst(firstWord.text)))
    //val newChild = .map({case (e,i) => if (i==firstWord) e.copy(child = Seq())}
    //)
    val newChildren = numberedChildren.map({
      case (c, i) => if (i == firstWordIndex) newFirstWord else c
    })
    d.copy(child = newChildren)
  }

  val xml = "@{http://www.w3.org/XML/1998/namespace}"

  /*
  def convert(folia: Node):Node =
  {
    val id = folia \ s"${xml}id"
*/

  def vuPatchName(w: Elem): Seq[Node] = {
    val txt = w.text


    val id = getId(w).getOrElse("noId")
    val typ = (w \ "@type").text
    val s1 = if ((w \ "@type").text.startsWith("01") && namenMap.contains(txt)) {
      val replacement = namenMap(txt).split("\\s+").toSeq
      val n = replacement.size
      val tagje = if (n > 1) "SPEC(deeleigen)" else (w \ "@pos").text
      val sequence = replacement.zipWithIndex.flatMap({ case (t, i) => ({if (i >= 0) Text("\n") else Seq()} ++ <w xml:id={s"$id.part.$i"} type={typ} pos={tagje}>{t}</w>)
      })
      <name key={txt} resp="namenKlus">
        {sequence}
      </name> <note resp="namenKlus">De vu-naam was:
        {txt}
      </note>
    } else w
    s1
  }

  def vuPatch(d: Elem): Elem = {
    val v0 = updateElement5(d, x => x.label == "w", vuPatchName).head.asInstanceOf[Elem]
    val v1 = updateElement(v0, x => x.label == "s" && (x \ "w").nonEmpty, vuPatchS)
    v1
  }

  def useAutomaticTagging(d: Elem, f: File) = {
    val f1 = f.getCanonicalPath.replaceAll("xml-with-word-ids", "xml-tagged")

    val d1 = XML.load(f1)
    val mappie = (d1 \\ "w").map(w => {
      val id = getId(w)
      id.get -> w.asInstanceOf[Elem]
    }).toMap


    def pronomExtra(w: Elem):Elem =
    {
      val t1 = pronominabel.enhancePronFeatures(word=w.text, lemma=(w \ "@lemma").text, tag = (w \ "@pos").text)
      w.copy(attributes = w.attributes.filter(_.key != "pos").append( new UnprefixedAttribute("pos", t1, Null)  ))
    }

    val genderRegex = "gender=(.*?)[,)]".r

    def getGender(pos: String):Option[String] = genderRegex
      .findFirstMatchIn(pos)
      .map(_.group(1))
      .map(g => {
        val zijd = if (g.matches(".*[mf].*")) Set("zijd") else Set.empty[String]
        val onz = if (g.matches(".*n.*")) Set("onz") else Set.empty[String]
        zijd ++ onz
      }).filter(_.size == 1).map(_.head)

    def doW(w: Elem):Elem = {
      val id = getId(w)
      val tweakedForPos = if (id.isDefined && mappie.contains(id.get)) {
        val w1: Elem = mappie(id.get)
        val pos = (w \ "@pos").text
        val w1Pos = (w1 \ "@type").text
        val word = w.text
        val w1Lemma = (w1 \ "@lemma").text

        // Console.err.println(s"$word $pos $w1Pos")
        val newPos = {
          if (pos.matches("N.*soort.*ev.*") && !pos.matches(".*(zijd|onz).*")  && w1Pos.matches("NOU-C.*gender=.*"))
            {
              val g = getGender(w1Pos)
              if (g.isDefined)
                addFeature(pos,"x-" + g.get)// gender uit corpustagging moet wel gewantrouwd worden
              else
                pos
            } else if (pos.matches("VNW.*") && Set("ze","zij").contains(word.toLowerCase) && w1Pos.contains("sg"))
            addFeature(pos, "x-ev") else
          if (pos.matches("VNW.*") && Set("ze","zij").contains(word.toLowerCase) && w1Pos.contains("pl"))
            addFeature(pos, "x-mv") else
          if (pos.matches("ADJ.*prenom.e-pred.*") && w1Pos.matches(".*prenom.*")) replaceFeature(pos, "prenom.e-pred", "x-prenom")
          else if (pos.matches("ADJ.*prenom.e-pred.*") && w1Pos.matches(".*=adv.*")) replaceFeature(pos, "prenom.e-pred", "x-vrij,e-pred")
          else if (pos.matches("WW.*(vd|od).*(prenom.*vrij|vrij.*prenom).*")) {
            if (w1Pos.matches(".*prenom.*"))
              replaceFeature(pos, "prenom.vrij|vrij.prenom", "x-prenom") else replaceFeature(pos, "prenom.vrij|vrij.prenom", "x-vrij")
          }
          else if (pos.matches("WW.*(vd|od).*(prenom.*nom|nom.*prenom).*")) {
            if (w1Pos.matches(".*prenom.*"))
              replaceFeature(pos, "prenom.vrij|vrij.prenom", "x-prenom") else replaceFeature(pos, "prenom.nom|nom.prenom", "x-nom")
          }
          else pos
        }
        val lemmaAdd:List[UnprefixedAttribute] = if ((w \ "@lemma").nonEmpty)
          List()
        else {

          val redPatch = if (word.equals("avonds")) "avond"
          else if (pos.matches("VNW.*")) {
            if (word == "'t") "het"
            else if (word == "'n") "een"
            else if (word == "'s") "de"
            else w1Lemma
          } else w1Lemma


          lemmaLog.write(s"Lemma patch in: $redPatch for $word, $pos\n")

          List(new UnprefixedAttribute("lemma", redPatch, Null))
        }

        w.copy(attributes = append(w.attributes.filter(_.key != "pos").append(new UnprefixedAttribute("pos", newPos, Null)),lemmaAdd))
      } else w

      tweakedForPos
    }

    updateElement(d, _.label == "w", w => pronomExtra(doW(w)))
  }

  def getId(n: Node): Option[String] = n.attributes.filter(a => a.prefixedKey.endsWith(":id") ||
    a.key.equals("id")).map(a => a.value.toString).headOption


  def noNotes(d: Elem) = updateElement5(d, _.label == "note", n => Seq()).head.asInstanceOf[Elem]

  def doFile(f: File, fOut: File): Unit = {
    val doc = XML.loadFile(f)
    (doc \\ "w").map(_.asInstanceOf[Elem])
    val d1 = updateElement(doc, x => x.label == "w" || x.label == "pc", x => if (x.label == "w") doWord(x) else doPunct(x))

    val d2 = updateElement(d1, _.label == "name", doName)

    //val d2a = updateElement(d1, _.label == "p", p=> p.copy())

    val d3 = if (Set("camb", "cgtl1", "cgtl2").contains(f.getParentFile().getName))
      vuPatch(d2) else d2

    val d4 = updateElement(d3, _.label == "p", e => e.copy(child = <s>
      {e.child}
    </s>))

    val d5 = useAutomaticTagging(d4.asInstanceOf[Elem], f)

    val d6 = noNotes(d5)

    lazy val d7 = updateElement(d6, _.label == "w", e => replaceAttribute(e, "pos", orderFeatures ))

    XML.save(fOut.getCanonicalPath, d7, "UTF-8")
  }

  def replaceAttribute(e: Elem, name: String, f: String=>String):Elem = {

    val s = (e \ ("@" + name)).text
    val s1 = f(s)
    e.copy(attributes = e.attributes.filter(_.key != name).append(   new UnprefixedAttribute(name, s1, Null)))
  }

  def doFile(f: String, f1: String): Unit = doFile(new File(f), new File(f1))

  val m0 = <x y="1"/>.attributes.filter(p => p.key != "y")
  println(m0)

  def append(m: MetaData, l: List[UnprefixedAttribute]): MetaData = {
    val m1 = if (m == Null) m0 else m


    l.foldLeft(m1)((m, u) => m.append(u))
  }

  import util.matching.Regex._

  def findVuPos(pos: String): (Int, Int, Int) = {
    val vuPos0: Int = "vu ([0-9]{3})".r.findFirstMatchIn(pos).map(m => m.group(1)).getOrElse("999").toInt

    val vuPos = vuPos0 % 1000
    val vuMainPos = (vuPos - vuPos % 100) / 100
    val vuSub1 = vuPos - 100 * vuMainPos
    val vuSubPos = (vuSub1 - vuSub1 % 10) / 10
    val vuSubSubPos = vuPos - 100 * vuMainPos - 10 * vuSubPos

    (vuMainPos, vuSubPos, vuSubSubPos)
  }

  def vuPosRaw(pos: String):String =
    "vu ([0-9]+)".r.findFirstMatchIn(pos).map(m => m.group(1)).getOrElse("999")

  def doPunct(pc: Elem): Elem = {
    val id = getId(pc).get
    <pc xml:id={id} pos="LET()">{pc.text}</pc>
  }

  def doWord(w: Elem): Elem = {
    val word = w.text

    val lemma0 = (w \\ "@lemma").text
    val lemma1 = if (lemma0 == "_") "" else lemma0

    val pos = (w \\ "@pos").text

    val (vuMainPos, vuSubPos, vuSubSubPos) = findVuPos(pos)

    val rawVuPos = vuPosRaw(pos)

    val (lemma, supply): (String, Boolean) = {
      if (lemma1 == "" && vuMainPos == 0 && vuSubSubPos == 0) (word, true); else (lemma1, false)
    }


    val lemmaIsWord = new UnprefixedAttribute("lemma", lemma, Null)

    //Console.err.println(s"$vuPos $vuMainPos:$vuSubPos:$vuSubSubPos")

    val cert: UnprefixedAttribute = new UnprefixedAttribute("maybenot", "true", Null)

    val candidates = goodMap.get(word.toLowerCase())



    val mappedPoS = mapTag(rawVuPos)
    val cleanedAttributes = w.attributes.filter(a => !(a.key == "pos") && !(a.key == "lemma" && a.value.text == "_")).append(
      new UnprefixedAttribute("pos", mappedPoS, Null)
    ).append(new UnprefixedAttribute("type", rawVuPos, Null))

    // Console.err.println(cleanedAttributes)

    // code below is awful. Rewrite!!

    val extraAttributes: List[UnprefixedAttribute] =
      if (candidates.isDefined) {
        val c = candidates.get.filter(w => (lemma.isEmpty || w.lemma == lemma) && w.matches(vuMainPos, vuSubPos, vuSubSubPos))

        val molexPos = c.map(_.pos)

        val molexPosAttribute = if (molexPos.isEmpty || {
          val noMolexPos = true; noMolexPos
        }) List()
        else
          List(new UnprefixedAttribute("molex_pos", molexPos.mkString("|"), Null))

        val lemmaCandidates = c.filter(w => lemma.isEmpty).map(_.lemma).toSet


        val lemmaAttribute = new UnprefixedAttribute("lemma", lemmaCandidates.mkString("|"), Null)

        val lAdd = if (lemmaCandidates.isEmpty)
          if (supply) List(lemmaIsWord) else List()

        else List(lemmaAttribute)

        val withAccent = c.filter(w => noAccents(w.word) != w.word.toLowerCase())
        val withoutAccent = c.filter(w => noAccents(w.word) == w.word.toLowerCase())

        if (withAccent.nonEmpty) {
          val a0: UnprefixedAttribute = new UnprefixedAttribute("corr", withAccent.head.word, Null)
          val a1: UnprefixedAttribute = new UnprefixedAttribute("sic", word, Null)
          // Console.err.println(s"$word ($lemma) => $withAccent")

          if (withoutAccent.isEmpty) List(a0,a1) ++ lAdd ++ molexPosAttribute
          else {
            List(a0, a1, cert) ++ lAdd ++ molexPosAttribute
          }
        } else lAdd ++ molexPosAttribute
      } else if (supply) List(lemmaIsWord) else List.empty[UnprefixedAttribute]
    val w1 = addDetailsToPos(w.copy(attributes = append(cleanedAttributes, extraAttributes)))
    val w2 = if ((w1 \ "@corr").nonEmpty && (w1 \ "@cert").isEmpty)
    w1.copy(child=Text( (w1 \ "@corr").text )) else w1
    w2
  }

  def addDetailsToPos(w: Elem): Elem = {
    val word = w.text
    val lemma = (w \ "@lemma").text
    val pos = (w \ "@pos").text
    val pos1 = addDetailsToPos(word, lemma, pos)
    val a = new UnprefixedAttribute("pos", pos1, Null)
    w.copy(attributes = w.attributes.filter(_.key != "pos").append(a))
  }

  def removeFeature(tag: String, feature: String):String = tag.replaceAll(s"([,(])$feature([,)])", "$1$2")
    .replaceAll(",+", ",").replaceAll(",\\)", ")")

  def replaceFeature(tag: String, f1: String, f2: String):String = tag.replaceAll(s"([,(])$f1([,)])", s"$$1$f2$$2")

  def removeFeatures(tag: String, features: Set[String]):String = features.foldLeft(tag)(removeFeature)

  def addFeature(tag: String, feature: String):String =
    {
      if (tag.contains(feature))
        tag else
      tag.replaceAll("\\)", s",$feature)").replaceAll("\\(,", "(")
    }

  def addFeatures(tag: String, features: List[String]):String = features.foldLeft(tag)(addFeature)

  val nodims = Set("franje", "plunje", "bonje")

  def addDetailsToPos(word: String, lemma: String, tag: String): String = {
    if (tag.matches("N.*soort.*")) {
      val withDim =
        if (verkleinvormen.contains(word.toLowerCase()))
        addFeatures(tag, List("dim","onz"))
      else if (word.endsWith("je") && !nodims.exists(word.toLowerCase.endsWith(_)))
        addFeatures(tag, List("dim","onz")) // voor onbekende woorden: lijstje
      else
        addFeature(tag, "basis")

      val withGender = if (withDim.contains("dim") || !geslachtenMap.contains(lemma.toLowerCase()) || geslachtenMap(lemma.toLowerCase()).contains(","))
        withDim
      else
        {
          addFeature(withDim, geslachtenMap(lemma.toLowerCase()))
        }

      return withGender
    }

    if (tag.matches(".*WW.*pv.*tgw.*") && tag.matches(".*[23].*") && lemma.endsWith("n")) {
      if (word.endsWith("t") && !lemma.endsWith("ten")) {
        return tag.replaceAll("\\)$", ",met-t)")
      }
    }

    if (tag.matches("VNW.*refl.*") && tag.matches(".*pr.*")) {
      if (lemma.contains("kaar") || word.contains("kander") || word.contains("kaar"))
        return tag.replaceAll("\\(.*?,", "(recip,")
      if (lemma.contains("zich"))
        return tag.replaceAll("\\(.*?,", "(refl,")
      else
        return tag.replaceAll("\\(.*?,", "(pr,")
    }

    if (tag.matches("VNW.*aanw.*prenom.*") && (lemma == "het" || lemma == "de")) {
      val t1 = removeFeatures(tag.replaceAll("VNW", "LID").replaceAll("aanw", "bep"), Set("prenom", "det", "zonder"))
      return t1
    }

    if (tag.matches("VNW.*onbep.*prenom.*") && (lemma == "een" || word=="'n")) { // lemma heeft ie hier og niet altijd
      val t1 = removeFeatures(tag.replaceAll("VNW", "LID"), Set("prenom", "det", "zonder"))
      return t1
    }

    if (tag.matches("VNW.*det.*") && pronominabel.gradables.contains(lemma))
      return tag.replaceAll("det", "grad")
    tag
  }

  def doName(n: Elem) = {
    n.copy(child = n.child.map(
      c => c match {
        case w: Elem if (w.label == "w") =>
          w.copy(attributes = w.attributes.filter(_.key != "pos").append(new UnprefixedAttribute("pos", "SPEC(deeleigen)", Null)))
        case x: Any => x
      }
    ))
  }

  import posmapping.ProcessFolder

  def main(args: Array[String]) = {
    val d = new File(xmlDir)
    val d1 = new File(outputDir)
    ProcessFolder.processFolder(d, d1, doFile)
    lemmaLog.close() // bleh
  }
}

object namenKlus {
  val xmlDir = Eindhoven.outputDir

  def extractNames(d: Elem): Seq[(String, String)] = (d \\ "name").map(n => {
    val txt = (n \\ "w").map(_.text)
    val t1 = txt.mkString("").trim.toLowerCase
    val t2 = txt.mkString(" ")
    t1 -> t2
  })

  def extractNameParts(d: Elem) = (d \\ "w").filter(x => (x \ "@type").text.startsWith("01")).map(
    x => {
      val txt = x.text.trim
      val t1 = txt.toLowerCase
      t1 -> txt
    }
  ) ++ extractNames(d)


  val allFiles = posmapping.ProcessFolder.processFolder(new File(xmlDir), identity).toSet.filter(_.isFile)
  val hansLoos = allFiles.filter(f => (Set("camb", "cgtl1", "cgtl2").contains(f.getParentFile().getName)))
  val hanzig = allFiles.diff(hansLoos)

  lazy val resolvedNames: Map[String, Set[String]] = hanzig.toStream.map(f => extractNameParts(XML.loadFile(f))).flatten.groupBy(_._1).
    mapValues(_.map(_._2).toSet)

  val unresolvedNames = hansLoos.flatMap(
    f => {
      val shortPath = f.getParentFile.getName + "/" + f.getName
      val d = XML.loadFile(f)
      (d \\ "w").filter(x => (x \ "@type").text.startsWith("01")).map((shortPath, _))
    }
  )

  def main(args: Array[String]): Unit = {
    //hansLoos.foreach(println)
    unresolvedNames.foreach(
      { case (f, n) => {
        val t = n.text
        val id = getId(n).getOrElse("?")
        val t1 = n.text.replaceAll("-", "")
        val candidates = resolvedNames.getOrElse(t1, Set.empty).mkString("|")
        println(s"$f $id $t -> $candidates")
      }
      }
    )
  }
}

object allTags {

  case class TaggedWord(word: String, kort: Option[String], lang: Option[String], vu: Option[String]) {
    override def toString() = s"$word,${vu.getOrElse("_")},${kort.getOrElse("_")},${lang.getOrElse("_")}"
  }

  def listAllTags() = files.flatMap(f => (XML.loadFile(f) \\ "w").map(n => TaggedWord(n.text, hvhKort(n), hvhLang(n), vuPos(n))))

  def vert() = listAllTags().foreach({ case TaggedWord(w, k, l, v) =>
    println(s"$w\t${k.getOrElse("_")}\t${l.getOrElse("_")}\t${v.getOrElse("_")}")
  })

  def byVuTag() = listAllTags().filter(w => w.vu.isDefined && w.vu.get.length == 3).groupBy(_.vu).mapValues(l => scala.util.Random.shuffle(l).toSet)

  //.map({ case (w,k,l,v) =>  s"$w\t${k}\t${l.getOrElse("_")}\t${v.getOrElse("_")}" }
  // )

  def byKorteTag = listAllTags().groupBy(_.kort).mapValues(l => scala.util.Random.shuffle(l).toSet)

  def splitLangetags: Stream[TaggedWord] = listAllTags().filter(_.lang.isDefined).flatMap(w => w.lang.getOrElse("").split("\\|").toList.map(l
  => w.copy(lang = Some(l.replaceAll("\\{.*?\\}", "").replaceAll("\\[.*?\\]", "").replaceAll("MTU.[0-9]*_L[0-9]+_", "")))))

  // splitLangetags.foreach(println)

  def byLangeTag = splitLangetags.groupBy(_.lang).mapValues(l => scala.util.Random.shuffle(l).toSet)


  def main(args: Array[String]) = {
    val files = List("vu", "kort", "lang").map(s => new FileWriter("/tmp/" + s + ".taginfo.txt"))
    val (vuFile, kortFile, langFile) = (files(0), files(1), files(2))

    byVuTag.toList.sortBy(_._1).foreach(
      {
        case (v, w) if (v.isDefined && v.get.length < 4) => {
          val allekortjes = w.filter(_.kort.isDefined).map(_.kort.get).toSet.mkString(",")
          vuFile.write(s"${v.get}\t${w.size}\t${w.take(5).map(_.word).mkString("  ")}\n")
        }

        case _ =>
      }
    )

    byKorteTag.toList.sortBy(_._1).foreach(
      {
        case (v, w) => {
          kortFile.write(s"${v.getOrElse("_")}\t${w.size}\t${w.take(5).map(_.toString).mkString("  ")}\n")
        }

        case _ =>
      }
    )

    byLangeTag.toList.sortBy(_._1).foreach(
      {
        case (v, w) => {
          langFile.write(s"${v.getOrElse("_")}\t${w.size}\t${w.take(5).map(_.toString).mkString("  ")}\n")
        }

        case _ =>
      }
    )
    files.foreach(_.close())

  }
}

// leeuwenberg et al 2016 minimally supervised approach for  synonym extraction with word embeddings