package posmapping
import scala.xml._
import java.io.File
import scala.util.matching.Regex._
import scala.xml._

object mapMiddelnederlandseTags extends mapMiddelnederlandseTagsClass(false)
object mapMiddelnederlandseTagsGys extends mapMiddelnederlandseTagsClass(true)

class mapMiddelnederlandseTagsClass(gysMode: Boolean) {
  val rearrangeCorresp = gysMode

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

  val tagMapping  = scala.io.Source.fromFile("data/getalletjes2cgn.txt").getLines().toStream
    .map(s => s.split("\\t")).map(x => x(0) -> x(1)).toMap

  val gysParticles = io.Source.fromFile("data/Gys/separates.corr.txt").getLines.map(l => l.split("\\t")).map(l => l(1) -> l(2) ).toMap

  val gysParticleLemmata = io.Source.fromFile("data/Gys/separates.corr.txt").getLines
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

  def updateTag(e: Elem):Elem =
  {
    val morfcodes = (e \ morfcodeAttribuut).text.split("\\+").toList

    val newPoSAttribuut = {val f = (e \ "@function").text; if (f.isEmpty) None else Some(Ѧ("pos", f))}

    val n = (e \ "@n").text

    val partPart:Option[Int] = morfcodes.zipWithIndex.find(_._1.contains("{")).map(_._2)

    val lemmata = (e \ "@lemma").text.split("\\+").toList

    // TODO aanvullen van de lemmata alleen bij scheidbare WW en ADV en directe opeenvolgingen?
    val completedLemmata = lemmata.zipWithIndex.map({case (l,i) =>
      if (partPart.map(_ == i) == Some(true)) gysFixPartLemma(n).getOrElse(l) else l
    })

    val newId = new PrefixedAttribute("xml", "id", "w." + n, Null)

    val isPartOfSomethingGreater = (e \ "@corresp").nonEmpty || morfcodes.exists(_.contains("{"))

    val cgnTags:List[String] = morfcodes.map(m => morfcode2tag(m, isPartOfSomethingGreater, n))

    val lemmataPatched = if (cgnTags.size <= lemmata.size) lemmata else {
      val d = cgnTags.size - lemmata.size
      val extra = (0 until d).map(x => "ZZZ")
      lemmata ++ extra
    }

    val completedLemmataPatched = if (cgnTags.size <= completedLemmata.size) completedLemmata else {
      val d = cgnTags.size - completedLemmata.size
      val extra = (0 until d).map(x => "ZZZ")
      completedLemmata ++ extra
    }

    val cgnTag = cgnTags.mkString("+")

    val newMSDAttribute = new UnprefixedAttribute(msdAttribuut, cgnTag, Null)

    val newatts0 = {
      val a = e.attributes.filter(a => !weg.contains(a.key)).append(newMSDAttribute)
      if (getId(e).nonEmpty || n.isEmpty) a else a.append(newId)
    }

    val stm = sterretjeMatje(e)

    val newAtts = if (newPoSAttribuut.isEmpty || maartenVersie) newatts0 else newatts0.append(newPoSAttribuut.get)

    val afterStm = stm.map(x => newAtts.append(x.attribute)).getOrElse(newAtts)

    val withCompletedLemma = afterStm.filter(_.key != "lemma").append(Ѧ("lemma", completedLemmataPatched.mkString("+")))

    val featureStructures = cgnTags.map(s => CGNMiddleDutch.CGNMiddleDutchTagset.asTEIFeatureStructure(s))
      .zipWithIndex
      .map({ case (fs,i) => fs.copy(
        child=fs.child ++ <f name="lemma"><string>{completedLemmataPatched(i)}</string></f> ++
          (if (lemmataPatched(i) != completedLemmataPatched(i)) <f name="deellemma"><string>{lemmataPatched(i).replaceAll("-","")}</string></f> else Seq()),
        attributes=fs.attributes.append(Ѧ("n", i.toString) ))} )

    e.copy(attributes = withCompletedLemma, child = e.child ++ featureStructures)
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
        val stermatten = (f1 \\ "w").filter(x => (x \ "@corresp").nonEmpty).groupBy(e => (e \ "@corresp").text)
        stermatten.values.foreach(l => Console.err.println(l.sortBy(e => (e \ "@n").text.toInt).map(show(_))))

        val sterMatMap = stermatten.mapValues(l => l.map(x => getId(x).get))

        def newCorresp(w: Elem): Elem = {
          if ((w \ "@corresp").nonEmpty) {
            val cor = (w \ "@corresp").text
            val moi = getId(w).get

            val lesAutres = sterMatMap(cor).toSet.diff(Set(moi))

            if (lesAutres.isEmpty)
              w else {
              val newCor = lesAutres.map(x => s"#$x").mkString(" ")
              val newAtts = replaceAtt(w.attributes, "corresp", newCor)
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

  def removeUselessWhite(n: Seq[(Node,Int)]) = n.filter({ case (x,i) =>  !(x.isInstanceOf[Text] && x.text.trim.isEmpty  &&  (i==0 || i == n.size-1)) })


  def wrapContentInSeg(w: Elem): Elem =
  {
    val children = w.child.zipWithIndex

    val groupedChildren = makeGroup[(Node,Int)](children, {case (c,i) => !(c.isInstanceOf[Text] || c.label == "hi" || c.label == "expan")})

    val newChildren = groupedChildren.flatMap(
      g => {

        if (g.map(_._1.text).mkString.trim.nonEmpty && g.forall( {case (c,i) => c.isInstanceOf[Text] || c.label == "hi" || c.label == "expan"})) {

          val contents = removeUselessWhite(g).map(_._1)
          val ctext:String = contents.text.trim.replaceAll("\\s+", " ")

          <seg type='orth'>{contents}</seg>

        }
        else g.map(_._1)
      }
    )
    w.copy(child = newChildren)
  }


  def fixFile(in: String, out:String) = XML.save(out, wordSplitting.splitWords(fixEm(XML.load(in))),  enc="UTF-8")

  def main(args: Array[String]) = utils.ProcessFolder.processFolder(new File(args(0)), new File(args(1)), fixFile)
}



