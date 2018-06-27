package CRM

import CRM.CRM2Xml.{kloekeByCode, optXML}
import brievenalsbuit.bab.{addFeature, idUnsafe, wwAnalyses}
import org.incava.util.diff.Difference
import utils.PostProcessXML.updateElement
import utils.alignment.comp
import utils.{Alignment, SimOrDiff}

import scala.collection.JavaConverters._
import scala.xml.{XML, _}


case class Location(kloeke_code1: String, kloeke_code_oud: String, plaats: String, gemeentecode_2007: String, gemeente: String,
                    streek: String, provincie: String, postcode: String, land: String, rd_x: String, rd_y: String, lat: String, lng: String,
                    topocode: String, std_spelling: String, volgnr: String)

object Location
{
  def makeLocation(a: Array[String]): Location =
    Location(a(0), a(1), a(2), a(3), a(4), a(5), a(6), a(7), a(8), a(9), a(10), a(11), a(12), a(13), a(14), a(15))

  def allKloekeCodes(f: String):Stream[Location] = scala.io.Source.fromFile(f).getLines().toStream.map(l => makeLocation(l.split("\\t")))
}


object ents {

  val entities:Map[String, String] = List(
    ("&komma;", ","),
    ("&excl;", "!"),
    ("&2periods;", ".."),

    ("u&uml;", "ü"),
    ("ouml;", "ö"),
    ("a&uml;", "ä"),
    ("y&uml;", "ÿ"),
    ("e&uml;", "ë"),
    ("v&uml;", "v̈"),
    ("&duitsekomma;", "/"),

    ("&super;", ""), // ahem nog iets mee doen, weet niet precies wat

    ("o&grave;", "ò"),
    ("&period;", "."),
    ("&semi;", ";"),
    ("&tilde;", "~"),

    //("&scheider;", 17664)
    ("&r;", "°"), // soms samen met vorige karakter, zie crm.xml
    ("&hyph;", "-"),
    ("&unreadable;", "?"),
    ("&colon;", ":"),
    ("&quest;", "?")
  ).toMap;

  import java.text.Normalizer
  def noAccents(s: String):String = Normalizer.normalize(s, Normalizer.Form.NFD).replaceAll("\\p{M}", "").toLowerCase.trim


  val entityPattern = entities.keySet.mkString("|").r
  def replaceEnts(s: String):String = entityPattern.replaceAllIn(s, m => entities(m.group(0)))
}

object Meta
{
  def interp(n:String, v:String):Elem  = <interpGrp type={n}><interp>{v}</interp></interpGrp>

  def locationFields(c: String):NodeSeq =
    optXML(
      kloekeByCode.get(c).map(
        l => Seq(
          interp("witnessLocalization_province", l.provincie),
          interp("witnessLocalization_place", l.plaats),
          interp("witnessLocalization_lat", l.lat),
          interp("witnessLocalization_long", l.lng),
        )
      ))
}

case class Meta(locPlus: String, status: String, kloeke: String, year: String, month: String, id: String)
{
  def idPlus:String = s"$locPlus.$id".replaceAll(s"^${status}_",s"_$status:")

  val metaWithNames = List(
    ("pid", uuid()),
    ("witnessIsOriginalOrNot", status),
    ("witnessLocalization_kloeke", kloeke),
    ("witnessYear_from", year),
    ("titleLevel1", id))

  def asXML:NodeSeq = <listBibl type="metadata">
    <bibl>
      {metaWithNames.map({case (k,v) => Meta.interp(k,v)})  }
      {Meta.locationFields(kloeke)}
    </bibl>
  </listBibl>

  def uuid():String =
  {
    val source = idPlus
    val bytes = source.getBytes("UTF-8")
    java.util.UUID.nameUUIDFromBytes(bytes).toString
  }
}

case class CRMTag(code: String, cgTag: String, cgnTag: String, description: String)

object CRM2Xml {
  val atHome = false
  val lumpIt = false
  val dir:String = if (atHome) "/home/jesse/data/CRM/" else "/mnt/Projecten/Taalbank/CL-SE-data/Corpora/CRM/"
  val outputDir = dir + "/TEI"
  val CRM:String = dir + "CRM14Alfabetisch.txt"
  val index:String = dir + "index"
  private val kloekeCodes = Location.allKloekeCodes(dir + "kloeke_cumul.csv")

  val kloekeByCode:Map[String, Location] = kloekeCodes.groupBy(_.kloeke_code1).mapValues(_.head)


  val squareCup = "⊔"

  lazy val tags:Stream[CRMTag] = scala.io.Source.fromFile("data/CG/allTags.overzichtje.tsv")
    .getLines.toStream.map(l => l.split("\\t"))
    .map(c => CRMTag(c(0), c(1), c(2), c(3)) )

  lazy val tagMap:Map[String,String] = tags.groupBy(_.code).mapValues(_.head.cgTag)

  // o_I222p30601    o       I222p   1306    01      StBernardHemiksem.Summarium113.VlpNr6
  //@ @ @ _o:I222p30601.StBernardHemiksem.Summarium113.VlpNr6 Markup(samp) - - -

  def optXML(x: Option[NodeSeq]):NodeSeq = if (x.isEmpty) <none/> else x.get


  def meta(c: Array[String]):Meta = { Meta(c(0), c(1), c(2), c(3), c(4), c(5))}


  def getRows(fileName: String):Stream[Array[String]] = scala.io.Source.fromFile(fileName).getLines.toStream.map(_.split("\\s+"))

  lazy val metaList:Stream[Meta] = getRows(index).filter(_.size > 5).map(meta)

  val metaMap:Map[String,Meta] = metaList.groupBy(_.idPlus).mapValues(_.head)

  lazy val rawTokens:Stream[Token] = getRows(CRM).zipWithIndex.map({case (x,i) => (x,i+1)}).filter({case (x,n) => x.size > 4}).map({ case (x,n) => token(n,x) })

  val puncMap:Map[String,String] =  <pc x=":">&amp;colon;</pc>
    <pc x="/">&amp;duitsekomma;</pc>
    <pc x="-">&amp;hyph;</pc>
    <pc x=",">&amp;komma;</pc>
    <pc x =".">&amp;period;</pc>
    <pc x=";">&amp;semi;</pc>
    <pc x="???">&amp;unreadable;</pc>.filter(x => x.label=="pc").map(x => x.text -> (x \ "@x").toString).toMap


  def rewritePunc(s:String):String = puncMap.getOrElse(s, s)

  def mapTag(codes: String):String = codes.split("\\+").map(c => tagMap.getOrElse(c, "U" + c)).mkString("+")

  val printWTags = true

  case class Token(n: Int, word: String, wordLC: String, wordExpanded: String, lemma: String, tag: String,
                   unclear: String = null, grouping: String = null, syntCode: String=null)
  {
    import ents._

    def isHeader:Boolean = word.equals("@") && !tag.equals("Markup(line)") && !tag.equals("Markup(sic)") && !isComment
    def isLine:Boolean = tag.equals("Markup(line)")
    def isSic:Boolean =  tag.equals("Markup(sic)")
    def isSeparator:Boolean = tag.equals("Markup(sep)")
    def isComment:Boolean = tag.equals("Markup(com)")

    def asXML:Node =
      if (isLine) <lb/>
      else if (isSic) <sic/> // hoort bij voorgaande woord
      else if (isComment) <note>{lemma}</note>
      else if (isSeparator) <milestone type="separator"/> // ?? wat is dit precies
      else if (tag.contains("Punc"))
        <pc>{rewritePunc(word)}</pc>
          else {
            val w = alignExpansionWithOriginal(replaceEnts(word), replaceEnts(wordExpanded))
            val corresp = if (grouping != null) Some(Text(grouping)) else None
            if ((w \\ "choice").nonEmpty) Console.err.println(s"BUMMER: $word / $wordExpanded / $w")
            if (printWTags) <w xml:id={s"w.$n"} corresp={corresp} lemma={lemma} type={tag} pos={mapTag(tag)} orig={word} reg={wordExpanded}>{w}</w>
            else Text(w.text + " ")
          }
  }

  case class Document(id: String, tokens: List[Token], metadata: Option[Meta])
  {
    if (metadata.isEmpty)
      {
        // foutje op regel 172938 @ @ @ _n:sic Markup(sic) bedoeld.is.godes? - - #correctie: Markup en 'bedoeld is'  omgekeerd
        Console.err.println(s"ERROR: No metadata for $id AT ${tokens.head}!")
        System.exit(1)
      }
    lazy val sentences = makeGroup[Token](tokens.toStream, t => t.syntCode == "1")
  }

  def token(n: Int, c:Array[String]):Token = { Token(n, c(0), c(1), c(2), c(3), c(4))
    c.size match {
      case 5 => Token(n, c(0), c(1), c(2), c(3), c(4))
      case 6 => Token(n, c(0), c(1), c(2), c(3), c(4), c(5))
      case 7 => Token(n, c(0), c(1), c(2), c(3), c(4), c(5), c(6))
      case x:Int if x >= 8 => Token(n, c(0), c(1), c(2), c(3), c(4), c(5), c(6), c(7))
    }
  }

  def token(n: Int, s:String):Token = {
    val c = s.split("\\s+")
    token(n,c)
  }


  def makeGroupx[T](s: Stream[T], currentGroup:List[T], f: T=>Boolean):Stream[List[T]] =
  {
    if (s.isEmpty) Stream(currentGroup)
    else if (f(s.head))
      Stream.cons(currentGroup, makeGroupx(s.tail, List(s.head), f))
    else
      makeGroupx(s.tail, currentGroup :+ s.head, f)
  }

  def makeGroup[T](s: Stream[T], f: T=>Boolean):Stream[List[T]] =
  {
    makeGroupx[T](s, List.empty, f).filter(_.nonEmpty)
  }


  def alignExpansionWithOriginal(org0: String, expansion0: String, useLevenshtein: Boolean=false):NodeSeq =
  {
    val expansion = expansion0.replaceAll("~", "") // is er meestal uit, dus voor consistentie maar altijd doen

    val original = org0.replaceAll("~?<nl>", "↩").replaceAll("~", squareCup).replaceAll("_\\?", "?").replaceAll("\\?_", "?")

    if (original.toLowerCase == expansion.toLowerCase) return Text(original)

    val positionsOfTilde = "⊔|↩".r.findAllMatchIn(original).toStream.map(m => m.start).zipWithIndex.map(p => p._1 - p._2)
    val tildesAtPosition = "⊔|↩".r.findAllMatchIn(original).toStream.zipWithIndex.map(p => p._1.start - p._2 -> p._1.group(0)).toMap

    val o1 = original.replaceAll("[⊔↩]","")


    val lr: List[(String, String, Int)] = findAlignment(expansion, o1, useLevenshtein)

    val showMe = lr.map(x => {
      val z = if (x._1==x._2) x._1 else s"${x._1}:${x._2}"
      z }
    ).mkString("|")

    val pieces = lr.flatMap(
      { case (left,right,i) =>
      {
        //Console.err.println(s"$left -> $right")
        val K = positionsOfTilde.find(k => k >= i && i + left.length() > k)

        val space = if (K.isDefined) tildesAtPosition(K.get) else ""

        val spaceSeq = if (space=="") Seq() else Seq(Text(space))
        val leftWithSpace = if (K.isEmpty) left else left.substring(0,K.get-i) + space + left.substring(K.get-i)
        import ents._

        if (noAccents(left) == noAccents(right)) Seq(Text(leftWithSpace)) else

        if (left.equals("_"))
          spaceSeq ++ Seq(<expan>{right}</expan>)
        else if (left.equals("?"))
          spaceSeq ++ Seq(<expan cert="low">{right}</expan>)
        else if (right.isEmpty)
          spaceSeq ++ Seq(<orig>{left}</orig>)
        else
          spaceSeq ++ Seq(<choice><orig>{left}</orig><reg>{right}</reg></choice>)
      } }
    )

    if (original.toLowerCase != expansion.toLowerCase && ((pieces \\ "choice").nonEmpty || useLevenshtein))
    {
      // Console.err.println(s"L=$useLevenshtein ORG=$original EXPANSION=$expansion ALIGNMENT=$showMe ${pieces.mkString("")}")
    }
    if (!useLevenshtein && (pieces \\ "choice").nonEmpty)
    {
      // Console.err.println("retry with levenshtein!!!!")
      val pieces2 = alignExpansionWithOriginal(org0, expansion0, true)
      return pieces2
    }
    pieces
  }


  private def findAlignmentLCS(expansion: String, o1: String) = {
    val a = new Alignment[Char](comp)
    val (diffs, sims) = a.findDiffsAndSimilarities(o1.toList, expansion.toList)
    val dPlus = diffs.map(d => SimOrDiff[Char](Some(d.asInstanceOf[Difference]), None))
    val simPlus = sims.map(s => SimOrDiff[Char](None, Some(s)))

    val corresp = (dPlus ++ simPlus).sortBy(_.leftStart)
    //Console.err.println(s"[$original] [$expansion]")
    val lr = corresp.map(
      c => {
        //Console.err.println(c)
        val left = o1.substring(c.leftStart, c.leftEnd)
        val right = expansion.substring(c.rightStart, c.rightEnd)
        (left, right, c.leftStart)
      })
    lr
  }

  object myCost extends utils.Aligner.Cost
  {
    override def deleteCost(x: Char): Double = super.deleteCost(x)

    override def insertCost(x: Char): Double = super.insertCost(x)

    override def replaceCost(x: Char, y: Char): Double = {
      val s1 = ents.noAccents(x.toString)
      val s2 = ents.noAccents(y.toString)
      if (s1 == s2)
        0 else
      super.replaceCost(x, y)
    }
  }

  def findAlignmentLevenshtein(expansion: String, original: String):List[(String,String,Int)] =
  {
    val x = new utils.Aligner
    x.c = myCost
    val l:java.util.List[utils.Aligner.Chunk] = utils.Aligner.clunk(x.alignment(original, expansion))
    l.asScala.toList.map(c => (c.left, c.right, c.position))
  }

  def findAlignment(expansion: String, original: String, useLevenshtein: Boolean): List[(String,String,Int)] =
  {
    if (useLevenshtein)
      findAlignmentLevenshtein(expansion, original)
    else
      findAlignmentLCS(expansion, original)
  }


  def findSeparables(s: Seq[Token]):Seq[Token] =
  {
    val si = s.zipWithIndex
    val startPoints = si.filter({ case (t,i)  => t.grouping != null && t.grouping.startsWith("b")})
    val endPoints = si.filter({ case (t,i)  => t.grouping != null && t.grouping.startsWith("e")})
    val pairings:Seq[(Int,Int)] = startPoints.map(
      { case (t, i) =>
        val ep = endPoints.find({ case (t1, i1) => i1 > i })
        t.n -> ep.map(_._1.n)}
        ).filter(_._2.isDefined).map(x => x._1 ->  x._2.get)

    val s2x = pairings.map(x => x._1 -> s"${x._1}.${x._2}").toMap
    val e2x = pairings.map(x => x._2 -> s"${x._1}.${x._2}").toMap

    si.map({ case (t,i)  =>
        if (s2x.contains(t.n)) t.copy(grouping = s2x(t.n)) else
        if (e2x.contains(t.n)) t.copy(grouping = e2x(t.n)) else
          t.copy(grouping=null)
    })
  }

  def markWordformGroups(d: Elem):Elem =
  {
    val wordOrder = (d \\ "w").zipWithIndex.map({case (w,i) => w -> i}).toMap

    val stermatten = (d \\ "w").filter(x => (x \ "@corresp").nonEmpty).groupBy(e =>  (e \ "@corresp").text)
    // stermatten.values.foreach(l => Console.err.println(l.sortBy(e => (e \ "@n").text.toInt).map(show(_))))

    val partAssignments = stermatten.values.flatMap(l =>
    {
      val sorted:Seq[Node] = l.sortBy(wordOrder)
      val pos = (sorted.head \ "@pos").text
      val word = sorted.map(_.text).mkString(" ")
      val analysis = wwAnalyses.get(word)

      // if (Set("WW", "BW").contains(pos)) Console.err.println(word + " / " + (sorted.head \ "@lemma").text + " /  " + pos )

      sorted.zipWithIndex.map({case (w,i) =>
      {
        val part = if (i == 0) "I" else if (i==l.length-1) "F" else "M"
        w -> (part,i,analysis)
      }
      })
    }).toMap

    val sterMatMap = stermatten.mapValues(l => l.map(idUnsafe))

    def newCorresp(w: Elem): Elem = {
      if ((w \ "@corresp").nonEmpty)
      {
        val cor = (w \ "@corresp").text
        val id = idUnsafe(w)
        val setje = sterMatMap(cor).toSet.diff(Set(id))
        val newCor = setje.map(x => s"#$x").mkString(" ")
        val (part,partNumber,partAnalysis) = partAssignments(w)

        val word = w.text
        val partAttribute = new UnprefixedAttribute("part", part, Null)
        val nAttribute = new UnprefixedAttribute("n", partNumber.toString, Null)
        val oldPos = (w \ "@pos").text
        val newPos = if (true || partAnalysis.isEmpty || !oldPos.contains("WW")) if (oldPos.contains("deeleigen")) oldPos else addFeature(oldPos,"deel") else {
          val partDesc = if (word  == partAnalysis.get.verbalWordPart) "hoofddeel-ww" else "anderdeel-ww"
          addFeature(oldPos, partDesc)
        }
        val newPosAttribute = new UnprefixedAttribute("pos", newPos, Null)
        val newAtts = w.attributes.filter(_.key != "corresp").append( new UnprefixedAttribute("corresp", newCor, Null)).append(partAttribute)
        w.copy(attributes =  newAtts.filter(a => a.key != "n" && a.key != "pos").append(newPosAttribute).append(nAttribute))
      } else w
    }
    updateElement(d, _.label=="w", newCorresp)
  }

  val white = Text(" ")

  val clauseMap = Map(
    "1" -> "hoofdzin (1)",
    "2" -> "hervatting hoofzin na bijzin (2)",
    "4" -> "betrekkelijke bijzin ingeleid door betrekkelijk voornaamwoord (4)",
    "5" -> "betrekkelijke bijzin ingeleid door betrekkelijk bijwoord (5)",
    "6" -> "betrekkelijke bijzin ingeleid door betrekkelijk voornaamwoordelijk bijwoord (6)",
    "8" -> "voegwoordelijke bijzin (8)"
  )

  def clauseType(s: String) = clauseMap.getOrElse(s,s)

  def markClauses(s: Seq[Token]) = makeGroup[Token](s.toStream, t => t.syntCode != null && t.syntCode.matches("[0-8]"))

  def sentenceXML(s: Seq[Token]) =
  {
    val clauses = markClauses(s)
    <s>
      {clauses.map(c =>
        { val typ = clauseType(c.head.syntCode)
          <clause type={typ}>{c.map(_.asXML).map(e => Seq(e,white))}</clause>
        })}
    </s>
  }

  def process():Unit =
  {
    val documents:Stream[Document] = makeGroup[Token](rawTokens, t => t.isHeader)
      .map(l => Document(l.head.lemma, l.tail, metaMap.get(l.head.lemma)))

    val withMetadataOriginal = makeGroup[Document](documents, d => d.metadata.isDefined)
      .flatMap(g =>
        {
          val meta = g.head.metadata.get.copy(status="n")
          g.head :: g.tail.map(x => x.copy(metadata=Some(meta.copy(id=x.id))))
        }
      )



    val xmlDocs = withMetadataOriginal.map(
      d =>
        {

          <TEI xmlns="http://www.tei-c.org/ns/1.0">
            <teiHeader>
            <title>{d.id}</title>
              {optXML(d.metadata.map(_.asXML))}
            </teiHeader>
            <text>
              <body>
              {
                // d.tokens.map(_.asXML).map(e => Seq(e,white))
                <p>
                {d.sentences.map(findSeparables).map( s=> sentenceXML(s) )}
                </p>
                }
              </body>
            </text>
          </TEI>
        }
    )

    lazy val corpus = <teiCorpus>{xmlDocs}</teiCorpus>

    if (lumpIt) {
      val xml = CRM.replaceAll("txt$", "xml")
      XML.save(xml, corpus, "UTF-8")
    } else {
      xmlDocs.foreach(d => {
        val title = (d \\ "title").head.text.replaceAll("/", "_")
        val d1 = markWordformGroups(d)
        val outputFile = outputDir + "/" + title + ".xml"
        XML.save(outputFile, d1, "UTF-8")
      }
      )
    }
  }

  def main(args: Array[String]): Unit = {

   process()
  }
}
