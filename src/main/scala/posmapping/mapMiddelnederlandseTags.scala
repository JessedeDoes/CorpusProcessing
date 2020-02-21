package posmapping
import scala.xml._
import java.io.File

import scala.util.matching.Regex._
import scala.xml._
import database.DatabaseUtilities.Select
import database.{Configuration, Database}
import posmapping.VMNWdb.{LemmaWoordvorm, QuotationReference}

import scala.collection.immutable
import utils.PostProcessXML.updateElement4
import utils.Tokenizer

import scala.util.Try
/*
+----------------+-------------+------+-----+---------+-------+
| Field          | Type        | Null | Key | Default | Extra |
+----------------+-------------+------+-----+---------+-------+
| citaat_id      | int(11)     | NO   | MUL | 0       |       |
| ldb_lemma_nr   | int(11)     | NO   |     | 0       |       |
| citaat_plaats  | varchar(42) | YES  |     | NULL    |       |
| woordvormtekst | varchar(80) | NO   |     |         |       |
| woordvolgnr    | int(11)     | NO   |     | 0       |       |
| doku_nr        | varchar(10) | NO   |     |         |       |
| woordvorm_nr   | int(11)     | NO   | MUL | 0       |       |
| bewijs_vanaf   | int(11)     | YES  |     | NULL    |       |
| is_attestatie  | int(1)      | YES  |     | 0       |       |
| onset          | int(11)     | YES  |     | NULL    |       |
| is_clitic      | int(1)      | YES  |     | 0       |       |
+----------------+-------------+------+-----+---------+-------+

 */



object mapMiddelnederlandseTags extends mapMiddelnederlandseTagsClass(false)

object mapMiddelnederlandseTagsGys extends mapMiddelnederlandseTagsClass(true)
{
  val bronnenlijst = "/mnt/Projecten/Taalbank/Woordenboeken/VMNW/VMNWdb/Bronnenlijst/vmnw_bronnen.xml"

  lazy val bronMap: Map[String, (Node,Node)] = (XML.load(bronnenlijst) \\ "bron").toStream.map(
    b => (b \\ "docid").text -> {
      val b1 = XML.loadString("<bron><![CDATA[" + b.toString.trim + "]]></bron>")
      (b1,b)
    }
  ).toMap

  def addMetadataField(bibl: Elem, name: String, value: String): Elem =
  {
     // Console.err.println(s"$name = $value")
     bibl.copy(child = bibl.child ++ Seq(
        <interpGrp type={name}><interp>{value}</interp></interpGrp>
     ))
  }

  def addMeta(d: Elem, name: String, value: String): Elem =
  {
    updateElement(d, e => e.label=="listBibl" && getId(e).nonEmpty && getId(e).get == "inlMetadata", lb =>
      updateElement(lb, _.label == "bibl", x => addMetadataField(x, name, value) )
    )
  }

  def addMeta(d: Elem, extra: Map[String,String]): Elem =
  {
    updateElement(d, e => e.label=="listBibl" && getId(e).nonEmpty && getId(e).get == "inlMetadata", lb =>
      updateElement(lb, _.label == "bibl", x => extra.foldLeft(x)({ case (bibl, (n,v)) => addMetadataField(bibl,n,v)}) )
    )
  }


  def voegBronInfoToe(d: Elem): Elem = {
    val sourceID = ((d \\ "interpGrp").filter(g => (g \ "@type").text == "sourceID") \ "interp").text.replaceAll("corpusgysseling.","")
    val (brontxt, bronxml): (Node,Node) = bronMap.get(sourceID).getOrElse((<bron_not_found/>, <nee-echt-niet/>))

    val authenticity = (bronxml \\ "document" \\ "type").text
    val verblijfplaats = (bronxml \\ "document" \\ "verblijfplaats").text

    // Console.err.println(s"Authenticity: $authenticity")

    val m = Map("authenticityLevel1" -> authenticity, "signatureLevel1" -> verblijfplaats)

    updateElement(addMeta(d, m), _.label=="teiHeader", x => x.copy(child = x.child ++ Seq(brontxt)))
  }
}

class mapMiddelnederlandseTagsClass(gysMode: Boolean) {
  val rearrangeCorresp: Boolean = gysMode

  val squareCup = "⊔"

  def Ѧ(n:String, v: String) = new UnprefixedAttribute(n,v,Null)

  def updateElement(e: Elem, condition: Elem=>Boolean, f: Elem => Elem):Elem =
  {
    if (condition(e))
      f(e)
    else
      e.copy(child = e.child.map({
        {
          case e1: Elem => updateElement(e1,condition,f)
          case n:Node => n
        }
      }))
  }

  val partTranslations = Map(
     "deel" -> "part",
     "vz-deel-bw" -> "adp",
     "hoofddeel-bw" -> "adv",
     "bw-deel-ww" -> "adv",
     "hoofddeel-ww" -> "vrb",
     "deel-b" -> "initial",
     "deel-i" -> "internal",
     "deel-f" -> "final"
  )

  val tagMapping: Map[String, String] = scala.io.Source.fromFile("data/getalletjes2cgn.txt").getLines().toStream
    .map(s => s.split("\\t")).map(x => x(0) -> x(1)).toMap

  val gysParticles: Map[String, String] = io.Source.fromFile("data/Gys/separates.corr.txt").getLines.map(l => l.split("\\t")).map(l => l(1) -> l(2) ).toMap

  val gysParticleLemmata: Map[String, String] = io.Source.fromFile("data/Gys/separates.corr.txt").getLines
    .map(l => l.split("\\t"))
    .map(l => {
      val lem = l(4)
      val deel = l(2)
      val lemPatched = if (deel.contains("ww") || deel.contains("bw")) lem.replaceAll("\\|","") else lem.replaceAll("\\|","-")
      l(1) -> lemPatched }  )
    .toMap

  def gysFixPartLemma(n: String):Option[String] =
  {
    gysParticleLemmata.get(n)
  }

  def morfcode2tag(morfcode: String, isPart: Boolean, n: String):String =
    {
      val s0 = morfcode.replaceAll("\\{.*", "").replaceAll("ongeanalyseerd", "999").replaceAll("[A-Za-z]","")

      val s1 =  "0" * Math.max(0, 3 - s0.length) + s0

      val pos = tagMapping.getOrElse(s1, tagMapping("999")) // s"MISSING_MAPPING($s/($s1))")

      val posAdapted = // pas op deze code werkt alleen voor CRM!!!
        if (!isPart) pos else if (!gysMode) {
          if (pos.contains("WW")) {
            if (morfcode.equals("285")) "ADV(bw-deel-ww)" else pos.replaceAll("\\)", ",hoofddeel-ww)")
          } else if (pos.contains("BW"))
            { if (morfcode.equals("655")) "BW(adv-pron,vz-deel-bw)" else pos.replaceAll("\\)", ",hoofddeel-bw)") }
          else if (pos.contains("VZ"))
            pos.replaceAll("\\)", ",vz-deel-bw)")
          else if (pos.contains("ADJ")) pos.replaceAll("\\)", ",bw-deel-ww)") // PAS OP, CHECK DEZE
          else pos
        } else { // FOUT kijken WELK stukje de part heeft (of mogelijk hebben)
          val deelSoort = gysParticles.get(n)
          if (deelSoort.isEmpty || !morfcode.contains("{"))
            {
              // Console.err.println(s"Geen deelinfo gevonden voor $n!!!!")
              pos
            } else {
            val soort = deelSoort.get
            if (pos.contains("WW")) {
              if (soort == "bw-deel-ww") "ADV(bw-deel-ww)" else pos.replaceAll("\\)", ",hoofddeel-ww)")
            } else if (pos.contains("BW")) {
              if (soort == "vz-deel-bw") "BW(adv-pron,vz-deel-bw)" else pos.replaceAll("\\)", ",hoofddeel-bw)")
            } else pos.replaceAll("\\)", "," + "deel" + ")").replaceAll("\\(,", "(")
          }
        }

      posAdapted
    }

  val morfcodeAttribuut = "@type"
  val msdAttribuut = "msd"
  val posAttribuut = "pos"
  val functionAttribuut = "function"
  val maartenVersie = false

  val wegVoorMaarten = Set(posAttribuut, functionAttribuut, msdAttribuut)
  val wegVoorNederlab = Set(msdAttribuut, functionAttribuut)

  val weg = if (maartenVersie) wegVoorMaarten else wegVoorNederlab

  def getId(e: Node) = e.attributes.filter(_.key == "id").headOption.map(_.value.text)

  val stermat = "\\{([#*])([0-9]+)\\}".r

  case class SterretjeMatje(typ: String, id: String, lemma: String)
  {
    def attribute = new UnprefixedAttribute("corresp", "#" + id, Null)
  }

  def sterretjeMatje(w: Node) =
  {
    val code = (w \ morfcodeAttribuut).text
    val lemma = (w \ "@lemma").text
    stermat.findFirstMatchIn(code).map(m => SterretjeMatje(m.group(1), m.group(2), lemma))
  }

  def patchPoSMistakes(m: String, p:String): String =
  {
    if (m.startsWith("25") && p.contains("=finite")) p.replaceAll("=finite", "=inf") else p
  }

  def patchPosMistakes(morfcodes: List[String], pos: List[String]) = {
     val patchedPos = morfcodes.zip(pos).map{case (a,b) => patchPoSMistakes(a,b)}
     patchedPos.mkString("+")
  }

  def updateTag(e: Elem):Elem =
  {
    val morfcodes = (e \ morfcodeAttribuut).text.split("\\+").toList

    val newPoSAttribuut = {val f = (e \ "@function").text.replaceAll("auxiliary", "aux/cop");
      val f1 = patchPosMistakes(morfcodes, f.split("\\+").toList)
      if (f1.isEmpty) None else Some(Ѧ("pos", f1))}

    val n = (e \ "@n").text

    val partPart:Option[Int] = morfcodes.zipWithIndex.find(_._1.contains("{")).map(_._2)

    val lemmata = (e \ "@lemma").text.split("\\+").toList

    // TODO aanvullen van de lemmata alleen bij scheidbare WW en ADV en directe opeenvolgingen?
    val completedLemmata = lemmata.zipWithIndex.map({case (l,i) =>
      if (partPart.map(_ == i) == Some(true)) gysFixPartLemma(n).getOrElse(l) else l
    })

    val newId = new PrefixedAttribute("xml", "id", "w." + n, Null)

    val (dictionaryLinks:Seq[Node], lemmaPatches: Set[LemmaWoordvorm]) = if (gysMode) VMNWdb.linkXML(newId.value.text) else (Seq[Node](),Set[LemmaWoordvorm]())

    val hilexedLemmata: immutable.Seq[String] = if (!gysMode) completedLemmata else {
      val patches = lemmaPatches.filter(_.omspelling_uit_hilex.nonEmpty).groupBy(_.clitisch_deel_nr)
      lemmata.indices.map(i => if (!patches.contains(i)) completedLemmata(i) else {
        val possiblePatches = patches(i).map(_.omspelling_uit_hilex).map(_.get)
        val p2 = possiblePatches.flatMap(_.split("\\s*,\\s*").map(_.toLowerCase()).toSet) // voor gevalletjes "Heer, heer"
        if (possiblePatches.size > 1 || possiblePatches.isEmpty) {
          //Console.err.println(s"Ambiguity for modern lemma form ${completedLemmata(i)}: $possiblePatches")
          if (!p2.contains(completedLemmata(i).toLowerCase))
            {
              val p2 = possiblePatches.flatMap(_.split("\\s*,\\s*").map(_.toLowerCase()).toSet)
              //Console.err.println(s"Mismatch for lemma form ${completedLemmata(i)}, not in $possiblePatches")
            }
          completedLemmata(i)
        } else {
          val adapted = possiblePatches.mkString("|").split("\\s*,\\s").map(_.toLowerCase).toSet.mkString("|")
          // if (adapted.toLowerCase != completedLemmata(i).toLowerCase) Console.err.println(s"Adapt lemma: ${completedLemmata(i)} -> $adapted")
          adapted
        }
      }).map(_.toUpperCase())
    }

    val isPartOfSomethingGreater = (e \ "@corresp").nonEmpty || morfcodes.exists(_.contains("{"))

    val cgnTags:List[String] = morfcodes.map(m => morfcode2tag(m, isPartOfSomethingGreater, n))

    val cgnTagsAstags = cgnTags.map(t => CGNMiddleDutch.CGNMiddleDutchTagset.parser.parseTag(CGNMiddleDutch.CGNMiddleDutchTagset, t))

    val deeltjes = cgnTagsAstags.map(t => t.features.filter(_.name == "deel").map(_.value))
    val erZijnDeeltjes = deeltjes.exists(_.nonEmpty)

    val lemmataPatched = if (cgnTags.size <= lemmata.size) lemmata else {
      val d = cgnTags.size - lemmata.size
      val extra = (0 until d).map(x => "ZZZ")
      lemmata ++ extra
    }

    val completedLemmataPatched = if (cgnTags.size <= completedLemmata.size) hilexedLemmata else { // pas op nu hilexedLemmata gebruik ipv completedLemmata
      val d = cgnTags.size - hilexedLemmata.size
      val extra = (0 until d).map(x => "ZZZ")
      hilexedLemmata ++ extra
    }

    val cgnTag = cgnTags.mkString("+")

    val newMSDAttribute = new UnprefixedAttribute(msdAttribuut, cgnTag, Null)

    val newatts0 = {
      val a = e.attributes.filter(a => !weg.contains(a.key)).append(newMSDAttribute)
      if (getId(e).nonEmpty || n.isEmpty) a else a.append(newId)
    }

    val stm = sterretjeMatje(e)

    val newAtts = if (newPoSAttribuut.isEmpty || maartenVersie) newatts0 else newatts0.append(newPoSAttribuut.get)

    val attributesWithUpdatedStermatStuff = stm.map(x => newAtts.append(x.attribute)).getOrElse(newAtts)

    val withCompletedLemma = attributesWithUpdatedStermatStuff.filter(_.key != "lemma").append(Ѧ("lemma", completedLemmataPatched.mkString("+")))

    val gysTagsCHNStylewithAnnotatedWordParts: Seq[(String, Tag)] =  (e \ "@function").text.split("\\+").toList.zipWithIndex.map(
      {
        case (t,i) =>
          val p  = partTranslations.get(deeltjes(i).headOption.getOrElse("none")).getOrElse("none")
          val z = if (deeltjes(i).nonEmpty) t.replaceAll("\\)", s",wordpart=$p)") else t

          if (erZijnDeeltjes)
            {
              // System.err.println(cgnTags)
              // System.err.println(z)
              // System.err.println(e)
              // System.exit(1)
            }
          z
      }
    ) map(s => s -> CHNStyleTags.parseTag(s))

    val updatedGysPosAttribute = Ѧ("pos", gysTagsCHNStylewithAnnotatedWordParts.map(_._1).mkString("+"))

    val msd = updatedGysPosAttribute.value.text
    val multimainpos = msd.replaceAll("\\(.*?\\)", "")
    val multimainattribute = new UnprefixedAttribute("groupingMainPos", multimainpos, Null)


    val attributesWithUpdatedGysTags = withCompletedLemma.filter(_.key != "pos").append(updatedGysPosAttribute)
    val gysTagFS: Seq[Elem] = gysTagsCHNStylewithAnnotatedWordParts.map(t => CHNStyleTags.gysTagset.asTEIFeatureStructure(t._2))
    val cgnTagFs: Seq[Elem] = cgnTags.map(s => CGNMiddleDutch.CGNMiddleDutchTagset.asTEIFeatureStructure(s))

    if (deeltjes.exists(_.nonEmpty)) {
      //System.err.println("DEELTJES " + deeltjes)
      //System.err.println("DEELTJES.... " + gysTagsCHNStyle.map(_.toString))
      //System.err.println(gysTagFS)
    }

    def makeFS(n: Seq[Elem]) = n
      .zipWithIndex
      .map({ case (fs,i) => fs.copy(
        child=fs.child ++ <f name="lemma"><string>{completedLemmataPatched(i)}</string></f> ++
          (if (lemmataPatched(i) != completedLemmataPatched(i)) <f name="deellemma"><string>{lemmataPatched(i).replaceAll("-","")}</string></f> else Seq()),
        attributes=fs.attributes.append(Ѧ("n", i.toString).append(multimainattribute) ))} )

    val featureStructures: Seq[Elem] = makeFS(cgnTagFs) ++ makeFS(gysTagFS)

    e.copy(attributes = attributesWithUpdatedGysTags, child = e.child ++ featureStructures ++ dictionaryLinks)
  }


  def show(w: Node) = s"(${w.text},${(w \ "@lemma").text},${(w \ "@msd").text}, ${sterretjeMatje(w)})"

  def queryLemma(w: Node)  = s"[lemma='${w \ "@lemma"}']"

  def replaceAtt(m: MetaData, name: String, value: String) = m.filter(a => a.key != name).append(Ѧ(name, value))

  def fixType(w: Elem): Elem = // haal de sterretjematjes uit het attribuut en maak overal drie cijfers van
  {
    val normType = (w \ "@type").text
      .replaceAll("\\{.*?\\}","")
      .split("\\+")
      .map(t => t.replaceAll("[^0-9]",""))
      .map(t => (0 until Math.max(0, 3 - t.length)).map(x => "0").mkString + t)
      .mkString("+")
    val newType = replaceAtt(w.attributes, "type", normType)
    w.copy(attributes = newType)
  }

  def fixEm(d: Elem):Elem =
    {
      val f1 = updateElement(d, _.label=="w", updateTag)


      if (rearrangeCorresp) {
        val stermatten: Map[String, NodeSeq] = (f1 \\ "w").filter(x => (x \ "@corresp").nonEmpty).groupBy(e => (e \ "@corresp").text)
        //stermatten.values.foreach(l => Console.err.println(l.sortBy(e => (e \ "@n").text.toInt).map(show(_))))

        val sterMatMap: Map[String, immutable.Seq[String]] = stermatten.mapValues(l => l.map(x => getId(x).get))

        def newCorresp(w: Elem): Elem = {
          if ((w \ "@corresp").nonEmpty) {
            val cor = (w \ "@corresp").text
            val moi = getId(w).get

            val lesAutres = sterMatMap(cor).toSet.diff(Set(moi))
            val tous: Set[String] = (sterMatMap(cor).toSet)
            val lemmaRef = tous.map(x => s"#$x").mkString(" ")
            if (lesAutres.isEmpty)
              w else {
              val newCor = lesAutres.map(x => s"#$x").mkString(" ")
              val newAtts = replaceAtt(w.attributes, "corresp", newCor)
                .append(new UnprefixedAttribute("lemmaRef", lemmaRef, Null)) // beetje lelijk maar handig om ook te hebben als groupid?
              w.copy(attributes = newAtts)
            }
          } else w
        }

        updateElement(f1, _.label == "w", w => wrapContentInSeg(fixType(newCorresp(w))))
      } else updateElement(f1, _.label == "w", w => wrapContentInSeg(w))
    }

  def makeGroupx[T](s: Seq[T], currentGroup:List[T], f: T=>Boolean):Stream[List[T]] =
  {
    if (s.isEmpty) Stream(currentGroup)
    else if (f(s.head))
      Stream.cons(currentGroup, makeGroupx(s.tail, List(s.head), f))
    else
      makeGroupx(s.tail, currentGroup :+ s.head, f)
  }

  def makeGroup[T](s: Seq[T], f: T=>Boolean):Seq[List[T]] =
  {
    makeGroupx[T](s, List.empty, f).filter(_.nonEmpty)
  }

  def removeUselessWhite(n: Seq[(Node,Int)]) =
    n.filter({ case (x,i) =>  !(x.isInstanceOf[Text] && x.text.trim.isEmpty  &&  (i==0 || i == n.size-1)) }).map(
      {
        case (x:Text,i) if (i==0 || i == n.size-1) => (Text(x.text.trim),i)
        case (x,i) => (x,i)}
    )


  def wrapContentInSeg(w: Elem): Elem =
  {
    val children = w.child.zipWithIndex

    val groupedChildren = makeGroup[(Node,Int)](children, {case (c,i) => !(c.isInstanceOf[Text] || c.label == "hi" || c.label == "supplied" || c.label == "expan" || c.label=="c")})

    val newChildren = groupedChildren.flatMap(
      g => {

        if (g.map(_._1.text).mkString.trim.nonEmpty && g.forall( {case (c,i) => c.isInstanceOf[Text] || c.label == "hi" || c.label == "supplied" || c.label == "expan" || c.label == "c"})) {

          val contents: Seq[Node] = removeUselessWhite(g).map(_._1)
          val ctext:String = contents.text.trim.replaceAll("\\s+", " ")

          <seg type='orth'>{contents}</seg>

        }
        else g.map(_._1)
      }
    )
    w.copy(child = newChildren)
  }

  /* Deze haalt de expan weg!! */

  def brack2supplied(w: Elem) = {
    val seg = (w \ "seg")
    if (seg.isEmpty) w else {
      val theseg = (w \\ "seg").head.asInstanceOf[Elem]

      val childXML: String = theseg.child.map(x => x match {
        case t: Text => t.text.replaceAll("\\[", "<supplied>").replaceAll("\\]", "</supplied>")
        case _ => x
      }).mkString("")

      val newSeg = Try(XML.loadString("<seg type='orth'>" + childXML + "</seg>")) match {
        case scala.util.Success(x) => x
        case _ => theseg
      }

      if (theseg.text.contains("["))
        {
          // Console.err.println(s"$theseg => $childXML => $newSeg")
        }

      w.copy(child = w.child.map(
        {
          case e:Elem if e.label == "seg" => newSeg
          case x => x
        }
      ))
    }
  }

  def tokenizeOne(w: Elem): NodeSeq = {
    val seg = (w \ "seg")

    if (seg.isEmpty ||
      seg.text.contains("[") ||
      seg.text.contains("]") ||
      (w \ "@pos").text.matches(".*NUM.*") ||
       (w \ "@pos").text.matches(".*PD.*") && w.text.trim.toLowerCase.matches("^([.]?)[ij]([.]?)"))
    Seq(w)
    else {

      val theseg = (w \\ "seg").head.asInstanceOf[Elem]
      val brun = theseg.text.contains("brun")
      if (theseg.child.exists(_.isInstanceOf[Elem])) {
        if (brun) println(s"Tokenizing $seg")
        val z: Seq[(Int, Option[Elem], Node)] = theseg.child.zipWithIndex.toSeq.flatMap(
          { case (n, i) if (i ==0 && n.isInstanceOf[Text]) => {
            val t = Tokenizer.tokenizeOne(n.text)
            if (t.leading.nonEmpty)
              Seq(
                (-1, Some(<pc type="pre">{t.leading}</pc>), Text(t.leading)),
                  (0, None, Text(t.token)),
                  (0, None, Text(t.trailing))
              )
            else Seq((0, None, n))
          }
          case (n,i) if (i == theseg.child.size -1 && n.isInstanceOf[Text]) => {
            val t = Tokenizer.tokenizeOne(n.text)
            if (t.trailing.nonEmpty)
              Seq(
                (0, None, Text(t.leading)),
                (0, None, Text(t.token)),
                (1, Some(<pc type="post">{t.trailing}</pc>), Text(t.trailing))
              )
            else Seq((0, None, n))
          }
          case (n,i) => Seq((0, None, n))
          }
        )
        if (brun) println(z)
        if (!z.exists(_._1 != 0)) Seq(w) else
          {
            val newSeg = theseg.copy(child = z.filter(_._1 == 0).map(_._3))
            val w1 = w.copy(child = w.child.map(
              {
                case e:Elem if e.label == "seg" => newSeg
                case x => x
              }
            ))
            val pref = z.filter(x => x._1 == -1).map(_._2.get)
            val post = z.filter(x => x._1 == 1).map(_._2.get)
            val r = pref ++ Seq(w1) ++ post
            if (brun) println(w1.text)
            if (brun) println(r)
            r
          }
      } else {
        val token = (w \\ "seg").text.replaceAll("\\s+", " ")
        val t = Tokenizer.tokenizeOne(token)
        val leading = if (t.leading.nonEmpty) Some(<pc type="pre">
          {t.leading}
        </pc>) else None
        val trailing = if (t.trailing.nonEmpty) Some(<pc type="post">
          {t.trailing}
        </pc>) else None
        val word = if (t.token.nonEmpty) Some(w.copy(child = w.child.map(
          { case e: Elem if e.label == "seg" => <seg type="orth">{t.token}</seg>
          case x => x
          }
        ))) else None
        //val x = brievenalsbuit.
        Seq(leading, word, trailing).filter(_.nonEmpty).map(_.get)
      }
    }
  }

  def tokenize(d: Elem):Elem = {
    val f1 = updateElement4(d, _.label == "w", w => tokenizeOne(brack2supplied(w)))
    f1.head.asInstanceOf[Elem]
  }

  val splitWords = false

  def fixFile(in: String, out:String) = {
    if (true || in.contains("3000")) {
      val d1 = fixEm(XML.load(in))
      val d2 = if (splitWords) wordSplitting.splitWords(d1) else d1
      val d3 = tokenize(if (gysMode) mapMiddelnederlandseTagsGys.voegBronInfoToe(d2) else d2)
      XML.save(out, scanLinks.addScanLinks(d3).head.asInstanceOf[Elem], enc = "UTF-8")
    }
  }


  def main(args: Array[String]) = {

    utils.ProcessFolder.processFolder(new File(args(0)), new File(args(1)), fixFile)
  }
}


object tests extends mapMiddelnederlandseTagsClass(false) {
  def test1 = {
    val w = <w><seg>aap<expan>je</expan></seg></w>
    println(tokenizeOne(w))
  }

  def test2 = {
    val w =  <w lemma="CONFESSOR" function="NOU(type=common,number=sg,inflection=other)" type="9" n="1074665">conf<expan resp="editor">essoris.</expan>
    </w>
    //println(updateTag(w))
    val d = <doc>{w}</doc>
    println(tokenizeOne(brack2supplied(wrapContentInSeg(updateTag(w)))))
  }
  override def main(args: Array[String]) = {
    test2
    //utils.ProcessFolder.processFolder(new File(args(0)), new File(args(1)), fixFile)
  }
}


