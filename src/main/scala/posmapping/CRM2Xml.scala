package posmapping

import org.incava.util.diff.Difference
import utils.{AlignmentGeneric, SimOrDiff}
import utils.alignment.comp

import scala.xml._

object CRM2Xml {
  val atHome = true
  val dir = if (atHome) "/home/jesse/data/CRM/" else "/mnt/Projecten/Taalbank/CL-SE-data/Corpora/CRM/"
  val CRM:String = dir + "CRM14Alfabetisch.txt"
  val index:String = dir + "index"
  val squareCup = "⊔"

  case class Tag(code: String, cgTag: String, cgnTag: String, description: String)

  lazy val tags:Stream[Tag] = scala.io.Source.fromFile("data/CG/allTags.overzichtje.tsv")
    .getLines.toStream.map(l => l.split("\\t"))
    .map(c => Tag(c(0), c(1), c(2), c(3)) )

  lazy val tagMap:Map[String,String] = tags.groupBy(_.code).mapValues(_.head.cgTag)

  // o_I222p30601    o       I222p   1306    01      StBernardHemiksem.Summarium113.VlpNr6
  //@ @ @ _o:I222p30601.StBernardHemiksem.Summarium113.VlpNr6 Markup(samp) - - -

  def optXML(x: Option[Elem]):Elem = if (x.isEmpty) <none/> else x.get

  case class Meta(locPlus: String, status: String, kloeke: String, year: String, month: String, id: String)
  {
    def idPlus:String = s"$locPlus.$id".replaceAll(s"^${status}_",s"_$status:")
    println(idPlus)

    def interp(n:String, v:String):Elem  = <interpGrp type={n}><interp>{v}</interp></interpGrp>

    val metaWithNames = List(
      ("pid", uuid()),
      ("witnessLocalization_kloeke", kloeke),
      ("witnessYear_from", year),
      ("titleLevel1", id))

    def asXML:Elem = <listBibl type="metadata">
      <bibl>
        {metaWithNames.map({case (k,v) => interp(k,v)})  }
      </bibl>
    </listBibl>

    def uuid():String =
    {
      val source = idPlus
      val bytes = source.getBytes("UTF-8")
      java.util.UUID.nameUUIDFromBytes(bytes).toString
    }
  }


  def meta(c: Array[String]):Meta = { Meta(c(0), c(1), c(2), c(3), c(4), c(5))}

  def metaFromId(s: String) =
  {
    val c = s.split(":")
    val (status, rest) = (c(0), c(1))
  }

  def getRows(fileName: String):Stream[Array[String]] = scala.io.Source.fromFile(fileName).getLines.toStream.map(_.split("\\s+"))

  lazy val metaList:Stream[Meta] = getRows(index).filter(_.size > 5).map(meta)

  val metaMap:Map[String,Meta] = metaList.groupBy(_.idPlus).mapValues(_.head)

  lazy val rawTokens:Stream[Token] = getRows(CRM).filter(_.size > 4).map(token)

  val puncMap:Map[String,String] =  <pc x=":">&amp;colon;</pc>
    <pc x="/">&amp;duitsekomma;</pc>
    <pc x="-">&amp;hyph;</pc>
    <pc x=",">&amp;komma;</pc>
    <pc x =".">&amp;period;</pc>
    <pc x=";">&amp;semi;</pc>
    <pc x="???">&amp;unreadable;</pc>.filter(x => x.label=="pc").map(x => x.text -> (x \ "@x").toString).toMap


  def rewritePunc(s:String):String = puncMap.getOrElse(s, s)

  case class Token(word: String, wordLC: String, wordExpanded: String, lemma: String, tag: String)
  {
    def isHeader:Boolean = word.equals("@") && !tag.equals("Markup(line)")

    def isLine:Boolean = tag.equals("Markup(line)")

    def asXML:Elem =
      if (isLine)
        <lb/>
      else
      if (tag.contains("Punc"))
        <pc>{rewritePunc(word)}</pc>
          else {
            val w = alignExpansionWithOriginal(word, wordExpanded)
            <w lemma={lemma} type={tag} pos={tagMap.getOrElse(tag, tag)} orig={word} reg={wordExpanded}>{w}</w>
          }
  }

  case class Document(id: String, tokens: List[Token])
  {
    lazy val metadata:Option[Meta] = metaMap.get(id)
  }

  def token(c:Array[String]):Token = { Token(c(0), c(1), c(2), c(3), c(4)) }
  def token(s:String):Token = { val c = s.split("\\s+");  Token(c(0), c(1), c(2), c(3), c(4)) }


  def makeGroup[T](s: Stream[T], currentGroup:List[T], f: T=>Boolean):Stream[List[T]] =
  {
    if (s.isEmpty) Stream.empty
    else if (f(s.head))
      Stream.cons(currentGroup, makeGroup(s.tail, List(s.head), f))
    else
      makeGroup(s.tail, currentGroup :+ s.head, f)
  }

  def alignExpansionWithOriginal(original: String, expansion: String):NodeSeq =
  {
    if (original.toLowerCase == expansion.toLowerCase) return Text(original)
    val a = new AlignmentGeneric[Char](comp)
    val x = "~".r.findAllMatchIn(original).toStream.map(m => m.start).zipWithIndex.map(p => p._1 - p._2)
    val o1 = original.replaceAll("~","")
    val (diffs, sims) = a.findDiffsAndSimilarities(o1.toList, expansion.toList)
    val dPlus  = diffs.map(d => SimOrDiff[Char](Some(d.asInstanceOf[Difference]), None))
    val simPlus  = sims.map(s => SimOrDiff[Char](None, Some(s)))

    val corresp = (dPlus ++ simPlus).sortBy(_.leftStart)
    //Console.err.println(s"[$original] [$expansion]")
    val lr = corresp.map(
      c => {
        //Console.err.println(c)
        val left = o1.substring(c.leftStart, c.leftEnd)
        val right = expansion.substring(c.rightStart, c.rightEnd)
        (left,right,c.leftStart)
      })


    val pieces = lr.flatMap(
      { case (left,right,i) =>
      {
        //Console.err.println(s"$left -> $right")
        val K = x.find(k => k >= i && i + left.length() > k)
        val space = if (K.isDefined) squareCup else ""
        val spaceSeq = if (space=="") Seq() else Seq(Text(space))
        val leftWithSpace = if (K.isEmpty) left else left.substring(0,K.get-i) + space + left.substring(K.get-i)
        if (left.toLowerCase == right.toLowerCase) Seq(Text(leftWithSpace)) else
        if (left.equals("_"))
          spaceSeq ++ Seq(<expan>{right}</expan>)
        else if (left.equals("?"))
          spaceSeq ++ Seq(<expan cert="low">{right}</expan>)
        else
          spaceSeq ++ Seq(<choice><orig>{left}</orig><reg>{right}</reg></choice>)
      } }
    )
    if (original.toLowerCase != expansion.toLowerCase)
    {
      Console.err.println(s"$original $expansion ${pieces.mkString("")}")
    }
    pieces
  }

  def process():Unit =
  {
    val documents = makeGroup[Token](rawTokens, List.empty, _.isHeader)
      .filter(_.nonEmpty)
      .map(l => Document(l.head.lemma, l.tail))

    val white = Text(" ")
    val xmlDocs = documents.map(
      d =>
        {
          <TEI>
            <teiHeader>
            <title>{d.id}</title>
              {optXML(d.metadata.map(_.asXML))}
            </teiHeader>
            <text>
              <body>
              {d.tokens.map(_.asXML).map(e => Seq(e,white))}
              </body>
            </text>
          </TEI>
        }
    )
    val corpus = <teiCorpus>{xmlDocs}</teiCorpus>

    val xml = CRM.replaceAll("txt$", "xml")
    XML.save(xml, corpus, "UTF-8")
  }

  def main(args: Array[String]): Unit = {

   process()
  }
}
