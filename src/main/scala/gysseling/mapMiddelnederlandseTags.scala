package gysseling

import java.io.File

import gysseling.VMNWdb.LemmaWoordvorm
import posmapping._
import utils.PostProcessXML.updateElement4

import scala.collection.immutable
import scala.xml._
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


class mapMiddelnederlandseTagsClass(gysMode: Boolean) {
  val rearrangeCorresp: Boolean = gysMode
  val cgnMode = false
  val squareCup = "⊔"
  val addMSD  = false
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

  val tagMappingOld: Map[String, String] = scala.io.Source.fromFile("data/getalletjes2cgn.txt").getLines().toStream
    .map(s => s.split("\\t")).map(x => x(0) -> x(1)).toMap

  //
  val mappingNS = "data/CG/gystags_newstyle.txt"
  val mappingGoogleDrive = "data/CG/gysseling_mapping_from_google_drive.txt"
  val tagMappingNew = scala.io.Source.fromFile(mappingGoogleDrive).getLines().toStream
    .map(s => s.split("\\t")).map(x => x(0) -> x(1)).toMap



  val tagMapping = if (cgnMode) tagMappingOld else tagMappingNew

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








  def getTaggingFromMorfcodes(morfcodes: List[String], pos: List[String], w: String, lemmata: Seq[String]) = {
    val posFromMapping = morfcodes.zip(lemmata).map({case (m,l) =>
      val m1 = if (l.toLowerCase.equals("beide") && m.equals("321")) "301" else m
      tagMapping.getOrElse(m1, s"U$m1")})
     val patchedPos = morfcodes.zip(posFromMapping).zip(lemmata).map{case ((a,b),l) =>
       HistoricalTagsetPatching.patchPoSMistakes(a,b,w,l)}
     patchedPos.mkString("+")
  }

  def updateTag(e: Elem):Elem =
  {
    val wordform = e.text.trim

    val morfcodes = (e \ morfcodeAttribuut).text.split("\\+").toList
    val morfcodes_normalized = normType((e \ morfcodeAttribuut).text).split("\\+").toList

    val lemmata = (e \ "@lemma").text.split("\\+").toList

    // hier gaat het al fout
    val newPoSAttribuut = {val f = (e \ "@function").text.replaceAll("auxiliary", "aux/cop");
      val f1 = getTaggingFromMorfcodes(morfcodes_normalized, f.split("\\+").toList, wordform, lemmata)
      if (f1.isEmpty) None else Some(Ѧ("pos", f1))}

    val n = (e \ "@n").text

    val partPart:Option[Int] = morfcodes.zipWithIndex.find(_._1.contains("{")).map(_._2)

    // TODO aanvullen van de lemmata alleen bij scheidbare WW en ADV en directe opeenvolgingen?
    val completedLemmata = lemmata.zipWithIndex.map({case (l,i) =>
      if (partPart.map(_ == i) == Some(true)) gysFixPartLemma(n).getOrElse(l) else l
    })

    val newId = new PrefixedAttribute("xml", "id", "w." + n, Null)
    val noVMNW = false

    val (dictionaryLinks:Seq[Node], lemmaPatches: Set[LemmaWoordvorm]) =
      if (gysMode && !noVMNW) VMNWdb.linkXML(newId.value.text) else (Seq[Node](),Set[LemmaWoordvorm]())

    val hilexedLemmata: immutable.Seq[String] = if (!gysMode || noVMNW) completedLemmata
    else Hilex.adaptLemmataWithHilex(lemmata, completedLemmata, lemmaPatches)

    val isPartOfSomethingGreater = (e \ "@corresp").nonEmpty || morfcodes.exists(_.contains("{"))

    val cgnTags:List[String] = morfcodes.map(m => HistoricalTagsetPatching.morfcode2tag(tagMapping, m,
      isPartOfSomethingGreater, n, gysMode, gysParticles))

    lazy val cgnTagsAstags = cgnTags.map(t => CGNMiddleDutch.CGNMiddleDutchTagset.parser.parseTag(CGNMiddleDutch.CGNMiddleDutchTagset, t))

    lazy val deeltjes = cgnTagsAstags.map(t => t.features.filter(_.name == "deel").map(_.value))

    lazy val erZijnDeeltjes = deeltjes.exists(_.nonEmpty)

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
      val a0 =  e.attributes.filter(a => !weg.contains(a.key))
      val a = if (addMSD) a0.append(newMSDAttribute) else a0
      if (getId(e).nonEmpty || n.isEmpty) a else a.append(newId)
    }

    val stm = sterretjeMatje(e)

    val newAtts = if (newPoSAttribuut.isEmpty || maartenVersie) newatts0 else newatts0.append(newPoSAttribuut.get)

    val attributesWithUpdatedStermatStuff = stm.map(x => newAtts.append(x.attribute)).getOrElse(newAtts)

    val withCompletedLemma = attributesWithUpdatedStermatStuff.filter(_.key != "lemma").append(Ѧ("lemma", completedLemmataPatched.mkString("+")))

    // OK hier gaat het mis.....

    val np = if (newPoSAttribuut.isDefined) newPoSAttribuut.get.value.text  else (e \ "@function").text
    //println("np=" + np)
    val gysTagsCHNStylewithAnnotatedWordParts: Seq[(String, Tag)] =  np.split("\\+").toList.zipWithIndex.map(
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
    //println("Tjoep:" + gysTagsCHNStylewithAnnotatedWordParts)
    val updatedGysPosAttribute = Ѧ("pos", gysTagsCHNStylewithAnnotatedWordParts.map(_._1).mkString("+"))

    val msd = updatedGysPosAttribute.value.text
    val multimainpos = msd.replaceAll("\\(.*?\\)", "")
    val multimainattribute = new UnprefixedAttribute("groupingMainPos", multimainpos, Null)


    val attributesWithUpdatedGysTags = withCompletedLemma.filter(_.key != "pos").append(updatedGysPosAttribute)
    val gysTagFS: Seq[Elem] = gysTagsCHNStylewithAnnotatedWordParts.map(t => CHNStyleTags.gysTagset.asTEIFeatureStructure(t._2))
    lazy val cgnTagFs: Seq[Elem] = cgnTags.map(s => CGNMiddleDutch.CGNMiddleDutchTagset.asTEIFeatureStructure(s))

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

    val featureStructures: Seq[Elem] = (if (cgnMode) makeFS(cgnTagFs) else Seq())  ++ makeFS(gysTagFS)

    e.copy(attributes = attributesWithUpdatedGysTags, child = e.child ++ featureStructures ++ dictionaryLinks)
  }




  def show(w: Node) = s"(${w.text},${(w \ "@lemma").text},${(w \ "@msd").text}, ${sterretjeMatje(w)})"

  def queryLemma(w: Node)  = s"[lemma='${w \ "@lemma"}']"

  def replaceAtt(m: MetaData, name: String, value: String) = m.filter(a => a.key != name).append(Ѧ(name, value))

  def normType(txt: String) = {
    val hasType:Boolean = txt.matches(".*[0-9].*")

    val normTyp = if (!hasType) "999" else  txt
      .replaceAll("\\{.*?\\}", "")
      .split("\\+")
      .map(t => t.replaceAll("[^0-9]", ""))
      .map(t => (0 until Math.max(0, 3 - t.length)).map(x => "0").mkString + t)
      .mkString("+")

    normTyp
  }

  def fixType(w: Elem): Elem = // haal de sterretjematjes uit het attribuut en maak overal drie cijfers van
  {
    val txt = (w \ "@type").text

      val nt = normType(txt)
      Console.err.println(s"normType=$nt")
      val newType = replaceAtt(w.attributes, "type", nt)
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

        updateElement(f1, _.label == "w", w => GysselingTokenizer.wrapContentInSeg(fixType(newCorresp(w))))
      } else updateElement(f1, _.label == "w", w => GysselingTokenizer.wrapContentInSeg(w))
    }




  /* Deze haalt de expan weg!! */




  def tokenize(d: Elem):Elem = {
    val f1 = updateElement4(d, _.label == "w", w => GysselingTokenizer.tokenizeOne(GysselingTokenizer.brack2supplied(w)))
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
    println(GysselingTokenizer.tokenizeOne(w))
  }

  def test2 = {
    val w =  <w lemma="CONFESSOR" function="NOU(type=common,number=sg,inflection=other)" type="9" n="1074665">conf<expan resp="editor">essoris.</expan>
    </w>
    //println(updateTag(w))
    val d = <doc>{w}</doc>
    println(GysselingTokenizer.tokenizeOne(GysselingTokenizer.brack2supplied(GysselingTokenizer.wrapContentInSeg(updateTag(w)))))
  }
  override def main(args: Array[String]) = {
    test2
    //utils.ProcessFolder.processFolder(new File(args(0)), new File(args(1)), fixFile)
  }
}


