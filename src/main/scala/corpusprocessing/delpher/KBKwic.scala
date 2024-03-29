package scorpusprocessing.delpher

import corpusprocessing.delpher.QueryKB.{matchingDocumentIdentifiers, splitStream}
import corpusprocessing.delpher.{KBMetadata, Phrase, SRU, SingleTerm, TextQuery, Or}

import scala.xml._
import java.io._
import utils._


object KBKwic extends KBKwicker()
{

}

object DiamondKwic extends KBKwicker()
{
  override def containsOneOf(haystack:String, needles: Set[String]):Boolean =
    needles.exists(n => haystack.toLowerCase.equals(n.toLowerCase()))
}

case class KBKwicker()
{
  import Tokenizer._
  import SRU._
  import corpusprocessing.delpher.QueryKB._
  def window=20
  

  def containsOneOf(haystack:String, needles: Set[String]):Boolean =
    needles.exists(n => haystack.toLowerCase.contains(n.toLowerCase()))  // map(n => haystack.toLowerCase.contains(n.toLowerCase()) ).reduce((a,b) => a || b)


  def concordance(query:TextQuery, text:String):List[Concordance] =
  {
    val tokens = tokenize(text)

    val terms = SRU.termsIn(query).map(_.toLowerCase)

    val phrases: Seq[Phrase] = query.phrasesIn()

    //System.err.println(s"Phrases: $phrases")

    val matchPositions: List[(Int,Int)] = (0 to tokens.length-1).toList.filter(i =>  containsOneOf(tokens(i).token, terms) ).map(i => (i,i+1)) // dit anders (kunnen) doen

    def sliceTokenProperties(a:Int,b:Int):Map[String, Array[String]] =
      {
        val tokz = tokens.slice(a,b)
        Map( "prepunctuation" -> tokz.map(_.leading),
             "word" -> tokz.map(_.token),
             "postpunctuation" -> tokz.map(_.trailing) )
      }

    def startFor(p:Int) = Math.max(0,p-window)
    def endFor(p:Int) = Math.min(p+window,tokens.length)

    def conc( interval: (Int,Int) ) = {
      val (p,p1) = interval
      Concordance(p - startFor(p), p - startFor(p) + (p1-p), sliceTokenProperties(startFor(p), endFor(p1)), Map.empty, None)
    }

    //System.err.println(s"##################################### Concordancing: $matchPositions")

    def matching(c: Concordance) = phrases.exists(p => p.matches(c.tokenProperties("word").slice(c.hitStart, c.hitEnd)))
    collapseNeighbouring(matchPositions).map(conc).filter(matching)
  }

  def collapseNeighbouring(l: List[(Int,Int)]) :  List[(Int,Int)] = {
    if (l.isEmpty || l.size < 2) l else {
      val (s0,e0) = l.head
      val l1 = collapseNeighbouring(l.tail)
      val (s1,e1) = l1.head
      if (e0 >= s1)  {
        val r =   List((s0,e1)) ++ l1.tail
        // System.err.println(s"HA!!!!!!!!! collapsing to $r.... ")
        //System.exit(1)
        r
      }  else List(l.head) ++ l1
    }
  }

  def concordance(query:String,document:Node, meta:Node):List[Concordance] = concordance(SingleTerm(query), document, meta)

  def concordance(query:TextQuery, document:Node, meta: Node):List[Concordance] = concordance(query, document.text).map(c => c.copy(metadata=KBMetadata.getMetadata(meta)))

  def concordance(query:TextQuery, document:Node):List[Concordance] = concordance(query, document.text)

  def concordanceFile(query:String, fileName:String):List[Concordance] = { val d = XML.load(fileName); concordance(query, d , d) }
  
  def concordancesDir(query:String, dirName:String):List[Concordance] =
    new File(dirName).list().toList.par.flatMap(f => concordanceFile(query, dirName + "/" + f)).toList
  

  def concordanceURL(query:String, url:String, meta:Node):List[Concordance] = concordance(query,XML.load(url), meta)

  def concordanceURL(query:TextQuery, url:String, meta:Node):List[Concordance] = concordance(query,XML.load(url), meta)

  def kwicResults(s:String):Stream[Concordance] =
    matchingDocumentIdentifiers(s).flatMap( { case (id, metadataRecord) => concordanceURL(s, id, metadataRecord).toStream } )

  def concordanceDir(query:String, dirName:String):Unit = concordancesDir(query,dirName).foreach(println)

  def kwicResultsTagged(t:TextQuery)
  {
    val s0 = matchingDocumentIdentifiers(t)
    val split = splitStream(s0,100)
    split.par.foreach(
      x =>
        for ((id,metadataRecord) <- x)
        { println(KBKwic.concordanceURL(t, id, metadataRecord) .map( c => c.tag(chnTagger).vertical)) }
    )
  }

  def kwicResultsPar(t:TextQuery)
  {
      val s0 = matchingDocumentIdentifiers(t)
      val split = splitStream(s0,100)
      split.par.foreach(
           x =>  
             for ((id,metadataRecord) <- x)
             { println(KBKwic.concordanceURL(t, id, metadataRecord) /* .map( c => c.tag(chnTagger).vertical) */) }
      )
  }

  def kwicResultsParx(t:TextQuery):Stream[Concordance] =
  {
    val s0 = matchingDocumentIdentifiers(t)
    val split = splitStream(s0,100)
    split.par.flatMap(x =>
      (for ( (id,metadataRecord) <- x; c  <- KBKwic.concordanceURL(t, id, metadataRecord)) yield c).toStream
    ).toStream
  }

  def testPhrase() = {
    val p = Phrase(SingleTerm("rode"), SingleTerm("bosbes"))
    val p1 = Phrase(SingleTerm("bosbes"), SingleTerm("struik"))
    val c = concordance(Or(p,p1), "ik zie een rode, bosbes, struik")
    c.foreach(println)
  }

  def main(args:Array[String]):Unit = {
      val arg0 = if (args.length == 0) "bunzing" else args(0)
      if (args.length >= 2) concordanceDir(arg0,args(1)) else kwicResults(arg0).foreach(println)
    }
}

object testPhrase extends KBKwicker {
  override def main(args: Array[String]): Unit = testPhrase()
}