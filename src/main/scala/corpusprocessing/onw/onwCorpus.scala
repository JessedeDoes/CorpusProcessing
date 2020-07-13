package corpusprocessing.onw
import java.io.File
import java.util.Comparator

import corpusprocessing.onw.splitONWTagsInFeatures.punctuationType

import scala.xml._
import utils.PostProcessXML._
import utils.Tokenizer
import utils.alignment.AlignmentGeneric

import scala.util.matching.Regex

object onwCorpus {

  def getId(n: Node):Option[String] = n.attributes.filter(a => a.prefixedKey.endsWith(":id") || a.key=="id").map(_.value.toString).headOption

  import Settings._

  val extras = io.Source.fromFile(extra).getLines.map(l => l.split("\\t",-1).toList).toStream


  val articleId = extras.map(l => l(0) -> l(1)).filter(_._2.trim.nonEmpty).toMap

  val vulgaat = extras.map(l => l(0) -> l(2)).filter(_._2.trim.nonEmpty).toMap

  // vulgaat.foreach(println)



  var currentFile = "None"


  def addSomeInfo(w: Elem):Elem =
  {
    val idx = getId(w)
    val w1 =    if (idx.isDefined) {
        val id = idx.get.replaceAll("w\\.", "")
        val fs = (w \ "fs").head.asInstanceOf[Elem]
        val extra = articleId.get(id).map(aid => Seq(<f name="article">{aid}</f>)).getOrElse(Seq()) ++
          vulgaat.get(id).map(v => Seq(<f name="vulgaat">{v}</f>)).getOrElse(Seq())
        val fsNew = fs.copy(child = fs.child ++ extra)
        w.copy(child = w.child.filter(c => !(c.label == "fs")) ++ fsNew)
      } else w

    val lemma = (w \ "@lemma").text
    val lemmaClean = lemma.replaceAll(" .*","")
    // if (lemmaClean != lemma) Console.err.println(s"$lemma  ==> $lemmaClean")
    val w2 = w1.copy(attributes = w1.attributes.filter(_.key != "lemma").append(new UnprefixedAttribute("lemma", lemmaClean, Null)))
    tagset.mapTags(w2)
  }

  def extraWordInfo(d: Elem): Elem = updateElement(d, _.label=="w", addSomeInfo )

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



  def word(w: Elem)  = {
    val w0 = w.copy(child = w.child.filter(c => c.label != "fs" && c.label != "note"))
    if ((w \ "choice").nonEmpty) (w \\ "sic").text else w0.text.replaceAll("##[0-9a-z]+","").trim
  }

  def variants(w: Elem): Set[String] =
  {
    val w0 = w.copy(child = w.child.filter(c => c.label != "fs" && c.label != "note"))
    val z  = if ((w \ "choice").nonEmpty) (w \\ "sic" ++ w \\ "corr").map(_.text.toString).toSet else Set(w0.text.trim)
    z.map(_.toLowerCase.trim).map(s => Tokenizer.tokenizeOne(s).token)
  }

  def wStartsGroup(w: Elem, i : Int, s: Seq[(Elem, Int)]):Boolean =
  {

    if (i == 0 || w.label != "w") true
    else {
      val meerledig = (w \\ "f").filter(x => (x \ "@name").text == "meerledig" && x.text == "ja").nonEmpty

      if (!meerledig) true
      else {
        val prevElem = s(i - 1)._1
        if (prevElem.label != "w") true else {
          val wrd = word(w)
          val prevwrd: String = word(prevElem)
          // Console.err.println(s"$wrd $prevwrd")
          wrd != prevwrd && !(wrd.startsWith("|") && prevwrd.endsWith("|"))
        }
      }
    }
  }

  def removeUselessWhite(n: Seq[(Node,Int)]): Seq[(Node, Int)] = n.filter({ case (x,i) =>  !(x.isInstanceOf[Text] && x.text.trim.isEmpty  &&  (i==0 || i == n.size-1)) }).map(
    { case (x,i) =>
         if (n.size == 1 && x.isInstanceOf[Text]) (Text(x.text.trim),i)
         else if (i==0 && x.isInstanceOf[Text]) (Text(x.text.replaceAll("^\\s*","")),i)
        else if (i == n.size-1 && x.isInstanceOf[Text]) (Text(x.text.replaceAll("\\s*$","")),i)
         else (x,i) }
      )

  val noteRegex = "^(.*?)\\s*\\(..+\\)\\s*$"
  val noteR = new scala.util.matching.Regex(noteRegex)

  val nr = "^(.*?)\\s*\\((.*)\\)\\s*$".r

  def wrapContentInSeg(w: Elem): Elem =
  {
    if ((w \ "seg").nonEmpty) w else {


      val children = w.child.zipWithIndex

      val groupedChildren = makeGroup[(Node, Int)](children, { case (c, i) => !(c.isInstanceOf[Text] || c.label == "hi") })

      val newChildren = groupedChildren.flatMap(
        g => {
          if (g.map(_._1.text).mkString.trim.nonEmpty && g.forall({ case (c, i) => c.isInstanceOf[Text] || c.label == "hi" })) {
            val contents: Seq[Node] = removeUselessWhite(g).map(_._1)
            val ctext: String = contents.text.trim.replaceAll("\\s+", " ")
            val ok = !ctext.contains(" ") || ctext.matches("^\\p{L}+\\s*\\p{L}+$")
            val optwarning = if (ok) None else Some(Text("fishy"))
            if (g.forall({ case (c, i) => c.isInstanceOf[Text] }) && ctext.matches(noteRegex)) {
              val matx: Regex.Match = nr.findFirstMatchIn(ctext).get
              val (wtext, ntext) = (matx.group(1), matx.group(2))
              Seq(<seg type={optwarning}>
                {wtext.trim}
              </seg>, <note type="bijgelapt">
                {ntext}
              </note>)
            } else
              Seq(<seg type={optwarning}>
                {contents}
              </seg>)
          }
          else g.map(_._1)
        }
      )
      w.copy(child = newChildren)
    }
  }

  def removeSomeRedundancy(paragraph: Elem): Elem = {
    def nonWordFeatures = Set("taal", "bron", "addendum_datering", "commentaar")

    val collectNW: Set[(String, Set[String])] =
      nonWordFeatures.map(nwf =>
        nwf -> (paragraph \\ "fs").map(
          fs => (fs \ "f").filter(f => (f \ "@name").text == nwf).text
        ).toSet)

    val uniqueValues = collectNW.toSet.filter(_._2.size == 1)

    val omhoogjes = uniqueValues.map(_._1)


    def removeStuff(fs: Elem) = fs.copy(child = fs.child.filter(f => !omhoogjes.contains( (f \ "@name").text)))

    val interpjes = uniqueValues.map({
      case (n, v) => <interpGrp type={n}>
        <interp>
          {v.head}
        </interp>
      </interpGrp>
    })

    val stripped = updateElement(paragraph, _.label == "fs", removeStuff)
    val adorned = stripped.copy(child = interpjes.toSeq ++ stripped.child)
    adorned
  }

  object comp extends Comparator[(String,String)]
  {


    import java.text.Normalizer

    def flatten(string : String) = Normalizer.normalize(string, Normalizer.Form.NFD).replaceAll("\\p{M}", "").replaceAll("[|*\\]\\[]","");

    override def compare(t: (String,String), t1: (String,String)):Int =
      {
        flatten(t._2).compareToIgnoreCase(flatten(t1._2))
      }
  }


  def wrapWordContent(d: Elem) = updateElement(d,_.label=="w", wrapContentInSeg)


  val readingRegex = "\\([^()]* [^()]*\\)"
  val readingRegex2 = "\\((l\\.|Serrure|door de|er staat|d\\.w\\.z\\.|uit|hier ont| h verb|hs|of|DG|=|sic|afschr|DF|G|Q|Blok|interl|Margi|men|met|ne overbo|t.w.)[^()]* [^()]*\\)"

  val reading = readingRegex.r

  def findContext(div: Elem, debug:Boolean = true, isCorpusText:Boolean=true): Elem =
  {
    val context0 = (div \ "cit").filter(c => (c \ "@type").text == "context").text.trim

    val readings = reading.findAllMatchIn(context0)

    readings.foreach(m => {
      val g = m.group(0)
      val gm = readingRegex2.r.findFirstMatchIn(g)
      if (gm.isEmpty)
        Console.err.println(g)
    })

    val context = context0.replaceAll(readingRegex2, "")

    val taggedWords:Seq[Elem] = (div \\ "w").map(_.asInstanceOf[Elem])
    val tokenizedContext  = Tokenizer.tokenize(context)

    val after: Seq[Node] = Tokenizer.tokenize((div \ "after").text).flatMap(t => {
      val w = if (t.token.nonEmpty) <w after="after" xml:lang="und" msd="RES(type=foreign)" xml:id={"w.spec." + java.util.UUID.randomUUID.toString}>
        <seg>{t.token}</seg> <fs>
          <f name="pos.pos">RES</f> <f name="pos.type">foreign</f>
        </fs>
      </w> else Seq()
      val allPunct = t.leading + t.trailing
      val typ = punctuationType(allPunct)
      if (t.token.isEmpty)
        {
          if (allPunct.isEmpty) Seq() else <pc type={typ}>{allPunct}</pc>
        }
      else {
        val pre = if (t.leading.nonEmpty) <pc type="pre">{t.leading}</pc> else Seq()
        val post = if (t.trailing.nonEmpty) <pc type="post">{t.trailing}</pc> else Seq()
        pre ++ w ++ post
      }
    }
    ).toSeq

    val wordSequence = taggedWords.flatMap(
      w =>
        {
          val  id = getId(w).get
          word(w).split("\\s+").toList.map(Tokenizer.tokenizeOne(_).token).map(p => id -> p) // PAS OP KAN CONSEQUENTIES HEBBEN VOOR WAT EERDER GOED GING
        }
    ).toList

    val tokenSequence = tokenizedContext.map(_.token).zipWithIndex.map({case (t,i) => i.toString -> t}).toList

    if (false) Console.err.println(
      s"""
         |$wordSequence
         |$tokenSequence
       """.stripMargin)

    val a = new AlignmentGeneric(comp)


      // Console.err.println(s"$tokenSequence // $wordSequence")

      val chunks = a.findChunks(tokenSequence, wordSequence)

      val lr = chunks.map(
        c => {
          //Console.err.println(c)
          val left = tokenSequence.slice(c.leftStart, c.leftEnd)
          val right = wordSequence.slice(c.rightStart, c.rightEnd)
          (c.isSimilarity, left, right, c.leftStart)
        })


      val foundRelations:Map[Int,String] = lr.filter(_._1)
        .flatMap({ case (c,l,r,s) => l.zipWithIndex.map( {case (x,i) => {val y = r(i); x._1.toInt -> y._1} }) }  )
        .toMap

     // Console.err.println(foundRelations)

      val tokensWithReferenceToTaggedWords:Seq[Node] = tokenizedContext.zipWithIndex.map({
        case (t,i) =>
          val optWordId = foundRelations.get(i)
          val txt = t.leading + t.token + t.trailing
          if (optWordId.isDefined)
            <w sameAs={"#" + optWordId.get}>{txt}</w>
          else
            <w xml:lang="other" pos={specTag}>{txt}</w>
        })

      val tokensReplacedByTaggedWords:Seq[Node] = {
        val repl = tokenizedContext.zipWithIndex.map({
          case (t, i) =>
            val optWordId = foundRelations.get(i)
            val txt = t.leading + t.token + t.trailing
            if (optWordId.isDefined) {
              val taggedWord = taggedWords.find(w => getId(w).get == optWordId.get)
              taggedWord.getOrElse(<whaaaaaaaaaaaa/>)
            }
            else
              <w xml:lang="und" msd={specTag} xml:id={"w.spec." + java.util.UUID.randomUUID.toString}><seg>{txt}</seg><fs><f name="pos.pos">{specPOS}</f><f name={"pos." + specFeature}>{foreign}</f></fs></w>
        })
        utils.Stuff.uniq(repl)
      }

    /*
      val matchedWordIds: Seq[String] = lr.filter(_._1).flatMap( {case (b,l,r,s) => r.map(_._1) })
      val notMatched = words.filter(w => getId(w).isDefined && !matchedWordIds.contains(getId(w).get)).map(variants(_)).mkString(",")

      if (notMatched.nonEmpty) {
        Console.err.println(s"\n$currentFile\nAligned: ([$notMatched])\nW-woorden: ${wordSequence.map(_._2).mkString(" ")}\nContext: $context\n" +
          lr.map(x => s"{${x._1} [${x._2.map(_._2).mkString(" ")}] [${x._3.map(_._2).mkString(" ")}]}"))
        val matched =  words.filter(w => getId(w).isDefined && matchedWordIds.contains(getId(w).get))
        if (matched.isEmpty)
          Console.err.println("And BTW no *** word even appears in the quotation!")
      }
      */

     val mergedContext =  if (debug)
        tokensWithReferenceToTaggedWords
      else
        tokensReplacedByTaggedWords


// ‹I›




    val matched = tokensWithReferenceToTaggedWords.map(_ \ "@sameAs").map(_.text).map(_.replaceAll("#","")).toSet.filter(_.nonEmpty)

    //Console.err.println(matched)

    val notMatched = taggedWords.filter(w => getId(w).isDefined && !matched.contains(getId(w).get)).map(variants(_)).mkString(",")


    val hitCount = tokensWithReferenceToTaggedWords.count(e => (e \ "@sameAs").nonEmpty)
    val wordCount = taggedWords.size

    val ok = notMatched.isEmpty //  hitCount  >= wordCount

    val doublez = tokensWithReferenceToTaggedWords.groupBy(e => (e \ "@sameAs").text).filter({case (p,l) => l.size > 1})

    if (doublez.exists({ case (p,l) => p.nonEmpty && l.size > 0} ))
      {
        //Console.err.println(doublez)
      }

    //Console.err.println(s"$hitCount/$wordCount:   "  + dinges.toString.replaceAll("\\s+", " "))

    lazy val d2 = updateElement(div, x => x.label=="cit"  && (x \ "@type").text == "context", c => c.copy(child=mergedContext))
    lazy val d1 = updateElement(div, x => x.label=="p" || x.label=="l", c => c.copy(child=tokensReplacedByTaggedWords ++ after))


    if (isCorpusText)
      (if (debug) d2 else d1).copy(attributes = d1.attributes.append (new UnprefixedAttribute("cert", ok.toString + s": $notMatched", Null)) )
    else
      {
         val withGloss = 0

         val u1 = updateElement(div, x => x.label=="cit"  && (x \ "@type").text == "context",
           c => <cit>
             {c.child}
             <add resp="some crazy old Dutch guy">{taggedWords}</add>
           </cit>)
        updateElement2(u1,  x => x.label=="p" || x.label=="l", c => Seq()).asInstanceOf[Elem]
      }
  }

  def testContext(d: Elem, debug:Boolean =false, isCorpusText:Boolean=false) =
  {
    updateElement(d, _.label == "div1", e => findContext(e,debug,isCorpusText))
  }

  def groupSimilarDivs(body: Elem):Elem =
  {

    val divs:Seq[Elem] = (body \ "div1").map(_.asInstanceOf[Elem])
    val dᵢ = divs.zipWithIndex

    def divKey(d:Elem) =
      {
        val cits = (d \\ "cit").filter(c => (c \ "@type").text == "context")
        val bron:Set[String] = (d \\ "f").filter(f => (f \ "@name").text == "bron").map(_.text).toSet
        //(cits,bron)
        cits.text + (d \ "@n").text //  + ":" + bron.toList.minBy(identity).mkString(",") // deze match toleranter maken // maar alleen op citaat is riskant
      }

    def startsNewGroup(x:(Elem,Int) ):Boolean =
    {
      x match { case (d,i) =>
      i == 0 || ((d \ "@sameAs").isEmpty && {
      val dPrev = dᵢ (i - 1)
      ! (divKey (dPrev._1) == divKey (d) )
      })
      }
    }

    /*
    val metBronnenX = dᵢ.map({ case (d:Elem,i) =>
      val cits = (d \\ "cit")
      val bron:Set[String] = (d \\ "f").filter(f => (f \ "@name").text == "bron").map(_.text).toSet
      (bron,i)
    })
    */

    val grouped = makeGroup[(Elem,Int)](dᵢ, startsNewGroup  )// dᵢ.groupBy({case (d,i) => divKey(d)}).mapValues(l => l.sortBy(_._2))

    val vals = grouped // grouped.values.toSeq

    val newChildren = vals.sortBy(l => l.head._2).map(
      l => if (l.size == 1) l.head._1
      else
        {
          val h:Elem = l.head._1
          val otherWords = l.tail.map(_._1).flatMap(d => (d \\ "w"))
          val info = new UnprefixedAttribute("desc", s"Extra: ${l.tail.size}", Null)
          val peeFix = updateElement(h, x => x.label == "p" || x.label=="l", x => x.copy(child=x.child ++ otherWords))
          peeFix.copy(attributes = h.attributes.append(info))
        }
    )
    body.copy(child = newChildren)
    // val eerstelingen = dᵢ.filter( {case (d,i) => grouped(divKey(d)).head._2 == i})
  }

  def groupDivs(d: Elem) = updateElement(d, _.label=="body", groupSimilarDivs)

  def createWordformGroups(paragraph: Elem): Elem =
  {
    val childElements:Seq[Elem] = (paragraph.child).filter(_.isInstanceOf[Elem]).map(_.asInstanceOf[Elem])
    val wi = childElements.zipWithIndex
    val groups = makeGroup[(Elem,Int)](wi, {case (w,i) => wStartsGroup(w, i, wi)})

    val newChildren = groups.map(g => if (g.size == 1) g.head._1 else {
      val elems:Seq[Elem] = g.map(_._1)
      val newLemma = elems.map(_ \ "@lemma").map(_.text).mkString("+")
      val newPos = elems.map(_ \ "@pos").map(_.text).mkString("+")
      val newMSD = elems.map(_ \ "@msd").map(_.text).mkString("+")
      val lAtt = new UnprefixedAttribute("lemma",newLemma,Null)
      val posAtt = new UnprefixedAttribute("pos",newPos,Null)
      val msdAtt = new UnprefixedAttribute("msd",newMSD,Null)

      val featureStructures = elems.flatMap((_ \ "fs")).map(_.asInstanceOf[Elem]).zipWithIndex.map(
        {
          case (e, i) =>
            val wi = elems(i)
            val lemi = (wi \ "@lemma").text
            val posi = (wi \ "@pos").text
            e.copy(
              child = e.child ++ Seq(<f name="lemma">{lemi}</f>, <f name="pos">{posi}</f>),
              attributes = e.attributes.append(new UnprefixedAttribute("n", i.toString, Null)))
        })
      val w0 = elems.head

      val allWordForms = elems.map(_ \ "seg").map(_.text)
      val newWordform = <seg>{if (allWordForms.toSet.size==1) allWordForms.head else allWordForms.mkString.replaceAll("\\|+","")}</seg>

      w0.copy(label = "w",
        attributes = w0.attributes.filter(a => a.key != "pos" && a.key != "lemma" && a.key != "msd").append(lAtt).append(posAtt).append(msdAtt),
        child = Seq(newWordform) ++ w0.child.filter(x => x.label != "fs" && x.label != "seg") ++ featureStructures)
      // <groepje>{elems}</groepje>
    })
    removeSomeRedundancy(paragraph.copy(child = newChildren))
  }

  def fixWordformGroups(d: Elem) = updateElement(d, x => x.label=="p" || x.label=="l", createWordformGroups)

  def xuuid(b: String):String =
  {
    val source = b
    val bytes = source.getBytes("UTF-8")
    java.util.UUID.nameUUIDFromBytes(bytes).toString
  }

  def metaGedoe(d: Elem) =
  {
    val bibls = (d \\ "bibl").map(_.asInstanceOf[Elem])
    val dPid = ((bibls.head \\ "interpGrp").find(x => (x \ "@type").text == "pid").get \ "interp").text

    val uuidMap = bibls.map(b => b->xuuid(b.toString)).toMap

    val allIds = d.descendant.map(n => getId(n)).filter(_.isDefined).map(_.get).toSet
    val allInsts = bibls.filter(b => (b \\ "span").nonEmpty).map(b => (b \\ "span" \ "@inst").text.replaceAll("#","")).toSet

    Console.err.println(allInsts)

    // val usedSubBibls = bibls.filter(b => (b \\ "span").nonEmpty && allIds.contains((b \\ "span" \ "@inst").text.replaceAll("#","")))

    def replaceId(e: Elem) = {
      val id = getId(e)

      //  if (id.nonEmpty) { Console.err.println(id) }
      if (id.isDefined && id.exists(allInsts.contains(_)))
        e.copy(attributes = e.attributes.filter(_.key != "id").append(new UnprefixedAttribute("xml:id", "INT_" + id.get, Null)))
        else
        e
    }

    def replaceInst(e: Elem) = {
      val inst = (e \ "@inst").text
      // Console.err.println(e.label)
      if (inst.isEmpty)
        e else {
        Console.err.println("INST NOW: " + inst)
        val i0 = inst.replaceAll("#","")
        e.copy(attributes = e.attributes.filter(_.key != "inst").append(new UnprefixedAttribute("inst", "#INT_" + i0, Null)))
      }
    }

    def replaceBibl(b: Elem) =
    {
      val inst = (b \\ "span" \ "@inst").text

      val uuid = xuuid(dPid + "." + inst)

      val extraGrp = <interpGrp type="pid"><interp>{"INT_" + uuid}</interp></interpGrp>


      if (inst.isEmpty) b
      else
        {
          if (allIds.contains(inst.replaceAll("#","")))
            {
              val b0 = updateElement(b, _.label=="interpGrp", e => e.copy(
                attributes=e.attributes.append(new UnprefixedAttribute("inst", inst, Null))
              ))
              val b1 = b0.copy(child = Seq(extraGrp) ++ b0.child)
              updateElement(b1, e => (e \ "@inst").nonEmpty, replaceInst)
            } else {
            // Console.err.println("dropping unused bibl: " + b.toString)
            Seq()
          }
        }
    }

    val r0 = updateElement2(d, _.label=="bibl", replaceBibl).asInstanceOf[Elem]
    updateElement3(r0, x => true, replaceId)
  }

  val nice = false


  val corpusTexts = List("lw.xml",
    "mfr.reimb._a.xml",
    "mfr.reimb._b.xml",
    "mfr.reimb._c.xml",
    "wps_(dr._ea).xml",
    "wps_(hs._c).xml",
    "wps_(hs._fa).xml",
    "wps_(hs._h).xml",
    "wps_(hs._i).xml",
  )


  def sortDivjesByBron(body: Elem) =
  {
     val newChild = body.child.sortBy(
       n => {
         val bron = (n \\ "f").filter(f => ((f \ "@name")).text=="bron").headOption.map(_.text.trim).getOrElse("geen_bron")
         bron
       }
     )
    body.copy(child = newChild)
  }

  def sortDivjes(b: Boolean)(d: Elem):Elem = {
    updateElement(d, _.label=="body", sortDivjesByBron)
  }

  def fixFile(in: String, out:String): Unit =
    {
      val isCorpusText = true // corpusTexts.exists(in.contains(_))
      val debug = false
      val sortDiv1s = false // in.contains("act.fl")
      //if (!in.contains("few")) return ()

      val thingsToDo:List[Elem => Elem] = List (
        sortDivjes(sortDiv1s),
        extraWordInfo,
        wrapWordContent,
        groupDivs,
        fixWordformGroups,
        e => testContext(e,debug,isCorpusText),
        wordSpace,
        metaGedoe
      )

      currentFile = in.replaceAll(".*/","") // sorry effe niet functioneel voor logging....

      // val d = wordSpace(testContext(fixGroups(groupDivs(wrapWordContent(XML.load(in))))))

      val d = thingsToDo.foldLeft(XML.load(in))( (d,f) => f(d) )

      if (nice) {
        val p = new scala.xml.PrettyPrinter(300, 4)
        val t = p.format(d)
        val w = new java.io.PrintWriter(out)
        w.write(t)
        w.close()
      } else XML.save(out, d,  enc="UTF-8")
    }

  def wordSpace(d: Elem) = updateElement2(d, _.label=="w",w => Seq(Text("\n"), w)).asInstanceOf[Elem]


  def main(args: Array[String]) =
    {
      val argz = if (args.size >= 2) args else Array(inputDir,outputDir)
      utils.ProcessFolder.processFolder(new File(argz(0)), new File(argz(1)), fixFile)
    }
}

object wrapWordContent
{
  def fixFile(in: String, out:String): Unit =
  {
    val nice = true
    val thingsToDo:List[Elem => Elem] = List (
      onwCorpus.wrapWordContent
    )


    val d = thingsToDo.foldLeft(XML.load(in))( (d,f) => f(d) )

    if (nice) {
      val p = new scala.xml.PrettyPrinter(300, 4)
      val t = p.format(d)
      val w = new java.io.PrintWriter(out)
      w.write(t)
      w.close()
    } else XML.save(out, d,  enc="UTF-8")
  }

  def main(args: Array[String]) =
  {
    val argz = args
    utils.ProcessFolder.processFolder(new File(argz(0)), new File(argz(1)), fixFile)
  }
}
/*
val dinges = tokens.map(t => {
  val wOpt = words.find(e =>  (variants(e).contains(t.token.toLowerCase)))
  val txt = t.leading + t.token + t.trailing
  if (wOpt.isDefined)
    //wOpt.get
    <w sameAs={"#" + wOpt.get.attributes.find(_.key == "id").get.value.text}>{txt}</w>
  else
    <w xml:lang="other" pos="SPEC(vreemd)">{txt}</w>
}).toSeq
*/