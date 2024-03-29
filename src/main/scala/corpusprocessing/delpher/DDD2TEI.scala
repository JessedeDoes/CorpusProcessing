package corpusprocessing.delpher

import scala.xml._
import java.io.{File, PrintWriter}
import java.nio.file.Files
import java.nio.file.Path

import utils.{Tokenizer, zipUtils}

// krantenzips staan op http://www.delpher.nl/nl/platform/pages/helpitems?nid=498

import utils._

case class Component(n: Node)
{
  val dc = "@{http://purl.org/dc/elements/1.1/}"
  lazy val identifier = (n \ s"${dc}identifier").text
  lazy val isMetadata = identifier.endsWith("metadata")
  lazy val metadata = n \ "Resource" \  "dcx"

}

case class DIDL(d: Node)
{
  val dc = "@{http://purl.org/dc/elements/1.1/}"
  val ddd = "http://www.kb.nl/namespaces/ddd"

  lazy val componentMap = (d \\ "Component").map(c =>  (c \ s"${dc}identifier").text -> Component(c) )

  lazy val items = (d \\ "Item").map(DidlItem(_,this))

  lazy val issue:Option[DidlItem] = items.filter(_.isIssue).headOption

  lazy val issueMetadata:Option[Node] = issue.flatMap(_.metadataElement)

  lazy val articles = items.filter(x => x.isArticle && x.hasText)
}

case class DidlItem(e: Node, didl: DIDL)
{
  val dc = "@{http://purl.org/dc/elements/1.1/}"
  val dcx = "http://krait.kb.nl/coop/tel/handbook/telterms.html"

  lazy val identifier = (e \ s"${dc}identifier").text

  lazy val filenames:Seq[String] = (e \\ "Component" \\ "Resource" \\ s"@{$dcx}filename").map(_.toString)

  lazy val hasText:Boolean = filenames.nonEmpty

  lazy val componentMap = (e \\ "Component").map(c =>  (c \ s"${dc}identifier").text -> Component(c) )

  lazy val components = (e \ "Component").map(Component(_))

  lazy val  isArticle = identifier.matches(".*a[0-9]+$")
  lazy val  isPage = identifier.matches(".*p[0-9]+$")
  lazy val  isIssue = identifier.endsWith("mpeg21")

  lazy val  metadataElement:Option[Node] = (components.filter(_.isMetadata).flatMap(c => c.n \ "Resource" \  "dcx")).headOption

  lazy val hasMetadata: Boolean = metadataElement.isDefined

  lazy val metaPlus:Option[Node] = metadataElement.flatMap(x => didl.issueMetadata.map(m =>
    <meta>
      <article>
        {x.child}
      </article>
      <issue>
        {m.child}
      </issue>
    </meta>
  ))
}

object DIDL
{

  def main(args: Array[String]) =
  {
    val outRoot = zipUtils.getRootPath(args(1))
    zipUtils.find(args(0), p => p.endsWith("didl.xml")).par.foreach(d => doDidl(d,outRoot))
    // parallel in zip schrijven lijkt geen problemen te geven
    outRoot.getFileSystem.close
  }

  private def doDidl(didl:Path, outRoot: Path) =
  {
    val d:Elem = XML.load(Files.newInputStream(didl))
    val di = DIDL(d)
    di.articles.foreach(x =>
    {
      // println(s"${x.isArticle} ${x.identifier}  ${x.filenames} ${x.hasMetadata} ${x.metaPlus}")
      val file = didl.getParent.resolve(x.filenames(0))

      System.err.println(file)

      val articleTextElem = XML.load(Files.newInputStream(file))
      val e = toTEI.makeTEI(x.metadataElement.get.asInstanceOf[Elem],
        x.didl.issueMetadata.map(_.asInstanceOf[Elem]),
        articleTextElem,
        x.identifier)
      val wr = zipUtils.getWriter(outRoot,file.toString)
      wr.write(e.toString)
      wr.close
      // println(e)
    }
    ) // .e \\ "Component" \\ "Resource"))
  }
}

object Highlighting
{
  val z = "\\S+|\\s+".r

  def splitWithWhite(s:String):Seq[String] = z.findAllIn(s).toList

  def highlight(t:Node, f:String=>Boolean,triggered:Boolean=false):Seq[Node] =
  {
    if (t.isInstanceOf[Text])
      splitWithWhite(t.toString).map(w => if (f(w)) <b style="color:red; font-size:14pt"><u><i>{w}</i></u></b> else Text(w))
    else if (t.isInstanceOf[Elem])
    {
      val e = t.asInstanceOf[Elem]
      e.copy(child=e.child.flatMap(c => highlight(c,f,triggered || e.label=="text")))
    } else
      t
  }
}

object KBMetadata
{
  def getMetadata(x: Node):Map[String,String] =
  {
    val m:Option[Elem ]= if (x.label == "recordData")
      Some(x.asInstanceOf[Elem]) else (x \\ "recordData")
      .headOption.asInstanceOf[Option[Elem]]
    if (m.isDefined)
      m.get.child.map(x => (x.label, x.text)).toMap
    else
      Map.empty[String,String]
  }

  def getMetadata(fileName:String): Map[String,String] = getMetadata(XML.load(fileName))

  def getMetadataForAll(dir: String):Stream[Map[String,String]] =
    new File(dir).list.toStream.map(f => getMetadata(dir + "/" + f))

  def printAllMetadata(dir:String) =
    getMetadataForAll(dir).map(m => m.toList.map({ case (k,v) => k  + ":" + v } ).mkString("\t")).foreach(println)

  def main(args:Array[String]):Unit = printAllMetadata(args(0))
}

object toTEI
{
  import scala.util.matching._
  val Date = new Regex("^\\s*([0-9]{4})[^0-9]([0-9]{1,2})[^0-9]([0-9]{1,2}).*")

  def dateField(name:String, value:String): Elem =
    <interpGrp type={name}>
      <interp value={value}/>
    </interpGrp>

  def makeTEIWithoutMetadata(metadataRecord: Elem, text: Elem, id:String,  query: Option[SRUQuery]=None):Elem = makeTEI(metadataRecord, None, text, id, query)

  def makeInterpjes(x: Node) = x.child.filter(_.isInstanceOf[Elem]).map(x => <interpGrp type={x.label}><interp value={x.text}/></interpGrp>)

  def makeTEI(metadataRecord: Elem, issueMetadata: Option[Elem], text: Elem, id:String, query: Option[SRUQuery]=None, termsOfInterest: Set[String] = Set.empty[String]):Elem =
  {
    val plainText = text.text
    //scala.Console.err.println(plainText)

    val profile = BasicTextProfiler.profile(plainText)

    val lang = (if (profile("lang") != null) profile("lang") else "unknown") .toString

    // if (lang != "nl") println(s"$lang found for: $plainText")

    val profileXml = profile.map({
      case (n,v) => <interpGrp type={s"profile.$n"}>
        <interp value={if (v != null) v.toString else "unknown"}/></interpGrp>
    })

    //Console.err.println(profileXml)
    val cleanerId = id.replaceAll(".*urn=","")

    val mergedMeta =
      if (issueMetadata.isEmpty)
        metadataRecord
      else
      {
        val extras:Seq[Node] = issueMetadata.get.child.filter(c => c.isInstanceOf[Elem]
          && !metadataRecord.child.exists(p => p.label == c.label))
        <meta>
          {metadataRecord.child.filter(_.isInstanceOf[Elem])}
          {extras}
        </meta>
      }

    val Date(year,month,day) = (mergedMeta \ "date")(0).text
    val l = List( ("Year_",year), ("Month_",month),("Day_",day))
    val dataFieldsXML = l.map( { case (n,v) =>
      List("witness","text","pub")
        .map(t => List("from","to")
          .map(ft => dateField(t + n + ft, v))) })

    val title = (text \\ "title")(0).text // nee geen goed idee ..... (?)

    val interpjes = (mergedMeta.child.filter(_.isInstanceOf[Elem]))
      .map(x => <interpGrp type={x.label}><interp value={x.text}/></interpGrp>)

    val papertitle =
      if ((metadataRecord \ "papertitle").size > 0)
        (metadataRecord \ "papertitle")(0).text
      else if (issueMetadata.isDefined)
        (issueMetadata.get \ "title")(0).text
      else ""


    def getField(name: String) = (mergedMeta \\ name).map(n => <interp value={n.text}/>)


    val termsFound = (if (query.isDefined)
      {
        val terms = SRU.termsIn(query.get.query.textQuery).map(_.toLowerCase) ++ termsOfInterest.map(_.toLowerCase)

        //val hightightedText = Highlighting.highlight(text, s => terms.contains(s.toLowerCase()))
        // Console.err.println(terms)
        val hits = Tokenizer.tokenize(text.text).filter(t => terms.contains(t.token.toLowerCase())).map(_.token).toSet
        val hitCount = hits.size.toString

        Seq(<interpGrp type="query"><interp value={query.get.toString}>{query.get}</interp></interpGrp>,
          <interpGrp type="hits"><interp value={hits.mkString(",")}>{hits.mkString(",")}</interp></interpGrp>,
          <interpGrp type="hitCount"><interp value={hitCount}>{hitCount}</interp></interpGrp>)
      } else Seq())

    val moreMappedStuff = Seq(
      <interpGrp type="newspaperEdition">{getField("edition")}{getField("spatial")}</interpGrp>,
      <interpGrp type="medium"><interp value="krant"/></interpGrp>,
      <interpGrp type="publisher">{getField("publisher")}</interpGrp>,
      <interpGrp type="genre">{getField("type")}</interpGrp>,
      <interpGrp type="biblScope_page">{getField("page")}</interpGrp>,
      <interpGrp type="sourceArticle">{getField("source")}</interpGrp>,
      <interpGrp type="copyrighHolder">{getField("rights")}</interpGrp>,
      <interpGrp type="copyrightOwner">{getField("rights")}</interpGrp>
    ) ++ termsFound


    val textParent = if ((text \ "text").nonEmpty) (text \ "text")(0) else text

    val textContent = textParent.child

    val tei  =
    <TEI>
      <teiHeader>
        <fileDesc><titleStmt><title>{title}</title>
        </titleStmt><publicationStmt><p/></publicationStmt>
          <sourceDesc><p> </p>
            <listBibl id="inlMetadata">
              <bibl>
                {dataFieldsXML}
                <interpGrp type="titleLevel1">
                  <interp value={title}/>
                </interpGrp>
                <interpGrp type="titleLevel2">
                  <interp value={papertitle}/>
                </interpGrp>
                <interpGrp type="idno">
                  <interp value={cleanerId}/>
                </interpGrp>
                <interpGrp type="language">
                  <interp value={lang}/>
                </interpGrp>
                {moreMappedStuff}
              </bibl>
            </listBibl>
            <listBibl id="profile">
              {profileXml}
            </listBibl>{if (issueMetadata.isDefined) <listBibl id="issueMetadata">
            {makeInterpjes((issueMetadata.get))}
          </listBibl>}
            <listBibl id="articleMetadata">{makeInterpjes(mergedMeta)}
            </listBibl>
          </sourceDesc>
        </fileDesc>
      </teiHeader>
      <text>
        <body>
          {textContent}
        </body>
      </text>
    </TEI>
    //val tagged = chnTagger.taggedDocument(tei)
    //println(tagged)
    tei
  }

  val xmlFilter = new java.io.FilenameFilter { def accept(dir:File, name:String):Boolean =  name.toLowerCase.endsWith(".xml") }

  def save(dir:String,doc:Elem,fileName:String):Unit = { new PrintWriter(dir +"/" + fileName) { write(doc.toString()); close } }

  def convertToTEI(fromDir:String, toDir:String) =
  {
    val f = new File(fromDir)
    if (f.isDirectory)
    {
      val str = f.listFiles(xmlFilter).toStream
      val z = for {
        fi <- str
        n = fi.getName
        x = XML.load(fromDir + "/" + n)
        id = (x \\ "identifier").text
        meta = (x \\ "recordData")(0).asInstanceOf[Elem]
        tei = makeTEIWithoutMetadata(meta, x, id)
      }
        yield
          save(toDir, tei, n.replaceAll("xml", "tei.xml"))
      z.foreach( Nil=>Nil )
    }
  }

  def main(args:Array[String]):Unit =
  {
    convertToTEI(args(0), args(1))
  }
}

/*


Toevoegen:

<interpGrp type="medium"><interp value="krant"/></interpGrp>


Mappen:

<interpGrp type="spatial"><interp value="Landelijk"/> + </interpGrp><interpGrp type="edition"><interp value="Dag"/></interpGrp> wordt:  

<interpGrp type="newspaperEdition"><interp value="dag"/><interp value="landelijk"/></interpGrp>



<interpGrp type="publisher"><interp value="Bureau der Nederlandsche Staats-courant"/></interpGrp> wordt (=blijft) <interpGrp type="publisher"><interp value="Bureau der Nederlandsche Staats-courant"/></interpGrp>

<interpGrp type="type"><interp value="artikel"/></interpGrp> wordt <interpGrp type="genre"><interp value="artikel"/></interpGrp>

<interpGrp type="page"><interp value="193"/></interpGrp> wordt <interpGrp type="biblScope_volume"><interp>193</interp></interpGrp>

<interpGrp type="source"><interp value="Koninklijke Bibliotheek"/></interpGrp> wordt <interpGrp type="sourceArticle"><interp value="Koninklijke Bibliotheek"/></interpGrp>

<interpGrp type="rights"><interp value="Koninklijke Bibliotheek Den Haag"/></interpGrp> wordt zowel <interpGrp type="copyrighHolder"><interp value="Koninklijke Bibliotheek Den Haag"/></interpGrp> als <interpGrp type="copyrightOwner"><interp value="Koninklijke Bibliotheek Den Haag"/></interpGrp>



*/
