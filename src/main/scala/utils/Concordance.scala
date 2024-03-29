package utils

import scala.xml._

case class Capture(name:String, captureStart:Int, captureEnd: Int)

case class Concordance(hitStart: Int, hitEnd: Int,
                       tokenProperties:  Map[String,Array[String]],
                       metadata: Map[String,String],
                       captures:Option[List[Capture]] = None, realStart: Option[Int] = None, realEnd: Option[Int] = None)
{
  val minEen:Int  = -1
  val defaultProperty = "word"
  
  lazy val words = tokenProperties(defaultProperty)
  lazy val left = words.slice(0,hitStart).mkString(" ")
  lazy val right = words.slice(hitEnd, words.length).mkString(" ")
  lazy val hit = words.slice(hitStart, hitEnd).mkString(" ")
  
  def apply(field: String):Array[String] = tokenProperties(field)
  def meta(field:String) = metadata(field)
  
  def retokenize(t:Tokenizer) = 
  {
    val tokens = tokenProperties("word").map(s => t.tokenize(s).head)
    val prepunct = tokens.map(t => t.leading)
    val postpunct = tokens.map(t => t.trailing)
    val words = tokens.map(t => t.token)
    val newProperties = tokenProperties -- List("word","prepunctuation","postpunctuation") ++ List("word" -> words, "prepunctuation" -> prepunct, "postpunctuation" -> postpunct)
    val r = this.copy(tokenProperties=newProperties)
    //Console.err.println("retokenized:" + r)
    r
  }
  
  def vertical =
  {
    val keys = tokenProperties.keys.toList

    def prefix(i:Int) = if (i==hitStart) ">" else ""
    (0 to tokenProperties("word").size-1).map( 
        i => prefix(i) + keys.map(k => tokenProperties(k)(i)).mkString("\t") 
     ).mkString("\n")
  }
  
  /**
   * Problem: tagger will retokenize, so we have to match back the hit position. Rather ugly, and might not always work
   */
  
  def tag(implicit tagger:Tagger):Concordance = 
  {
    val retokenized =
      if (this.tokenProperties.contains("prepunctuation"))
        this
      else
        this.retokenize(Tokenizer)
    // .retokenize(Tokenizer) // nee, dubbel tokenizeren is niet goed....
    //Console.err.println(retokenized)
    val tagged = tagger.tag(retokenized("word").mkString(" "))
    if (hitStart >= retokenized("word").length)
      Console.err.println(s"Miserie: $hitStart $retokenized ${retokenized("word").length}")

    val findMe = retokenized("word")(hitStart)
    val indexes = (0 to tagged("word").size -1).filter(tagged("word")(_) == findMe)
    if (indexes.isEmpty)
    {
        Console.err.println(s"Tokenization misery after tagging: Unable to find $findMe in ${tagged("word").mkString(" ")}")
        this.copy(hitStart=minEen,hitEnd=minEen,tokenProperties=tagged)
    }
    else
    {
    val bestIndex = indexes.minBy(i => Math.abs(hitStart - i))
    val r = this.copy(hitStart=bestIndex,hitEnd=bestIndex+1,tokenProperties=tagged)
    r
    }
  }

  def toXML:Elem =
  {
    <Concordance>
      <hitStart>{hitStart}</hitStart>
      <hitEnd>{hitEnd}</hitEnd>
      <tokens>
        {tokenProperties.map( {case (k,v) => <layer type={k}>{v.zipWithIndex.map( {case (s,i) => <token n={i.toString}>{s}</token>})}</layer>})}
      </tokens>
      <metadata>
        {metadata.map( {case (k,v) => <property><key>{k}</key><value>{v}</value></property>} )}
      </metadata>
    </Concordance>
  }

  def snippet = f"$left%80s" + " \u169b"  + hit + "\u169c " + right

  override def toString:String = snippet + "\n\t\t" + (if (metadata.nonEmpty) metadata.values.mkString(";") else "")

  def toHTMLRow:Node = <tr align="right"><td>{left}</td><td><b>{hit}</b></td><td>{right}</td></tr>
  def toHTMLItem:Node = <li>{left} <b>{hit}</b> {right}</li>
 }

object Concordance
{
  def toHTMLTable(c: List[Concordance]):Node = <table>{c.map(_.toHTMLRow)}</table>
  def toHTMLList(c: List[Concordance]):Node = <ul>{c.map(_.toHTMLItem)}</ul>
  def toHTMLAsText(c: List[Concordance]):String = toHTMLTable(c).toString.replaceAll("\\s+", " ")

  def fromXML(e: Elem):Concordance =
  {
    val meta = (e \ "metadata" \ "property").map(p => ((p \ "key").text, (p \ "value").text)).toMap
    val hitStart = (e \ "hitStart").head.text.toInt
    val hitEnd = (e \ "hitEnd").head.text.toInt
    val tokenProperties = (e \ "tokens" \ "layer").map(l => ((l \ "@type").text, (l \ "token").map(t => t.text).toArray)).toMap
    Concordance(hitStart, hitEnd, tokenProperties, meta)
  }

  def toXML(s: Seq[Concordance]):Elem =
  {
    <Snippets>
      {s.map(_.toXML)}
    </Snippets>
  }
}



