package posmapping

import scala.xml._

object dinges {
  val dir = "/mnt/Projecten/Taalbank/CL-SE-data/Corpora/CRM/"
  val CRM = dir + "CRM14Alfabetisch.txt"
  val index = dir + "index"

  // o_I222p30601    o       I222p   1306    01      StBernardHemiksem.Summarium113.VlpNr6
  //@ @ @ _o:I222p30601.StBernardHemiksem.Summarium113.VlpNr6 Markup(samp) - - -

  def optXML(x: Option[Elem]) = if (x.isEmpty) <none/> else x.get

  case class Meta(locPlus: String, status: String, kloeke: String, year: String, month: String, id: String)
  {
    def idPlus = s"$locPlus.$id".replaceAll("^o_","_o:")
    println(idPlus)

    def interp(n:String, v:String):Elem  = <interpGrp type={n}><interp>{v}</interp></interpGrp>

    val metaWithNames = List(


     ("witnessLocalization_kloeke", kloeke),
      ("witnessYear_from", year),
      ("titleLevel1", id))

    def asXML = <listBibl xml:id="inlMetadata">
      <bibl>
        {metaWithNames.map({case (k,v) => interp(k,v)})  }
      </bibl>
    </listBibl>
  }


  def meta(c: Array[String]) = { Meta(c(0), c(1), c(2), c(3), c(4), c(5))}

  def metaFromId(s: String) =
  {
    val c = s.split(":")
    val (status, rest) = (c(0), c(1))
  }

  lazy val metaList = scala.io.Source.fromFile(index).getLines.toStream.map(_.split("\\s+")).filter(_.size > 5).map(meta)

  val metaMap = metaList.groupBy(_.idPlus).mapValues(_.head)

  lazy val rawTokens = scala.io.Source.fromFile(CRM).getLines.toStream.map(_.split("\\s+")).filter(_.size > 4).map(token)

  val puncMap =  (<pc x=":">&amp;colon;</pc>
    <pc x="/">&amp;duitsekomma;</pc>
    <pc x="-">&amp;hyph;</pc>
    <pc x=",">&amp;komma;</pc>
    <pc x =".">&amp;period;</pc>
    <pc x=";">&amp;semi;</pc>
    <pc x="???">&amp;unreadable;</pc>).filter(x => x.label=="pc").map(x => x.text -> (x \ "@x").toString).toMap


  def rewritePunc(s:String) = puncMap.getOrElse(s, s)

  case class Token(word: String, wordLC: String, wordExpanded: String, lemma: String, tag: String)
  {
    def isHeader:Boolean = word.equals("@") && !tag.equals("Markup(line)")

    def isLine = tag.equals("Markup(line)")

    def asXML =
      if (isLine)
        <lb/>
      else
      if (tag.contains("Punc"))
        <pc>{rewritePunc(word)}</pc>
          else
      <w lemma={lemma} pos={tag} reg={wordExpanded}>{word}</w>
  }

  case class Document(id: String, tokens: List[Token])
  {
    lazy val metadata = metaMap.get(id)
  }

  def token(c:Array[String]) = { Token(c(0), c(1), c(2), c(3), c(4)) }
  def token(s:String) = { val c = s.split("\\s+");  Token(c(0), c(1), c(2), c(3), c(4)) }


  def makeGroup[T](s: Stream[T], currentGroup:List[T], f: T=>Boolean):Stream[List[T]] =
  {
    if (s.isEmpty) Stream.empty
    else if (f(s.head))
      Stream.cons(currentGroup, makeGroup(s.tail, List(s.head), f))
    else
      makeGroup(s.tail, currentGroup :+ s.head, f)
  }


  def process =
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
   process
  }
}
