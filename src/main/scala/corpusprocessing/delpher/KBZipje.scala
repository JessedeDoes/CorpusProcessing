package corpusprocessing.delpher

import java.nio.file.{Path, _}

import corpusprocessing.delpher.{Download}
import scorpusprocessing.delpher.KBKwic
import utils.{Tokenizer, zipUtils, _}

import scala.collection.JavaConversions._
import scala.collection.JavaConverters._
import scala.xml._
/*

Een zipje met pseudoTEI bestandjes afgeleid van KB artikelen

*/

class KBZipje(filename: String)
{
   import KBZipje._

   lazy val periods = 1800 until 1940 by 10

   lazy val root:Path = zipUtils.getRootPath(filename)

   lazy val metadata:Seq[(String, Map[String,String], Path)] =
     zipUtils.findPath(root).filter(p => p.toString.endsWith(".xml")).map(p =>
   {
     val node = XML.load(Files.newInputStream(p))
     val fileName = p.getFileName.toString
     (fileName, parseBibl(node), p)
   })

   //lazy val textQuery = this.queryTerms.toList.map(s => SingleTerm(s))
   // this caches too much, try different approach

   lazy val documentStream:Stream[() => Node] =
     zipUtils.findPath(root).filter(p => p.toString.endsWith(".xml")).map(p => () => XML.load(Files.newInputStream(p)))

   lazy val byDecade:Map[String, Seq[(String, Map[String, String], Path)]] =
    metadata.groupBy({case (f, m, p) => m("witnessYear_from").replaceAll(".$", "")}).mapValues(s => s.toList)

   lazy val decadeStats:Map[String,Int] = byDecade.map({case (d,l) => (d,l.size)})

   def sample(number: Int, extraFilter: Path=>Boolean):Seq[String] =
   {
     val perPeriod:Int = Math.round(number / periods.size.asInstanceOf[Double]).toInt

     def selection = byDecade.mapValues(l =>
       {
         val shuf = scala.util.Random.shuffle(l).toList
         shuf.filter({case (f,m,p) => extraFilter(p)}).take(perPeriod) }
     )
     selection.keySet.toList.sortBy(identity).foreach(k => println(s"$k -> ${selection(k).size}"))
     //selection.foreach(println)
     selection.values.flatMap(l => l.map(_._1)).toList
   }

   def getDoc(p:Path):Node  = XML.load(Files.newInputStream(p))

   def textContainsInstanceOf(n: Node, s:Set[String]):Boolean = (n \\ "text").text.split("\\s+").exists(str => s.contains(str))
   def textContainsLowercaseInstanceOf(n: Node, s:Set[String]):Boolean = textContainsInstanceOf(n, s.map(_.toLowerCase))

   lazy val queryTerms:Set[String] = if (properties.containsKey("query")) getProperty("query").split(",").toSet else Set()

   lazy val textQuery: TextQuery =
     if (this.getProperty("expandedQueryXML") != null &&  this.getProperty("expandedQueryXML").nonEmpty)
     {
        val elem = XML.loadString(this.getProperty("expandedQueryXML")).asInstanceOf[Elem]
        SRU.fromXML(elem)
      } else ListDisjunction(queryTerms.toList.map(SingleTerm(_)))

  //println(queryTerms)

   def hasLowercaseHits(n:Node):Boolean = textContainsLowercaseInstanceOf(n, queryTerms)
   def hasLowercaseHits(p:Path):Boolean = hasLowercaseHits(getDoc(p))

   def zipSelectionTo(newZip: String, select: Path => Boolean):Unit =
   {
     val sampledZipje = new KBZipje(newZip)
     zipUtils.findPath(this.root, select).foreach(
       p => { println(p); val os = zipUtils.getOutputStream(sampledZipje.root, p.getFileName.toString); Files.copy(p,os); os.close()})
     this.properties.foreach( {case (n,v) => sampledZipje.setProperty(n,v)})
     sampledZipje.setProperty("sampled", "true")
     sampledZipje.close()
   }

  lazy val properties: java.util.Properties =
  {
    val p:Path = zipUtils.getPath(root, "properties.txt")
    if (Files.exists(p)) {
      val props = new java.util.Properties
      val inStream = Files.newInputStream(p)
      props.load(inStream)
      inStream.close()
      props
    }
    else
      new java.util.Properties
  }

  def saveProperties():Unit = {
    val os = zipUtils.getOutputStream(root, "properties.txt")
    properties.store(os, null)
    os.close()
  }

  def close():Unit = this.root.getFileSystem.close()

  def setProperty(name: String, value: String):Unit = { properties.setProperty(name, value); saveProperties(); }

  def setQueryTerms(baseTerm: String):Unit = setProperty("query", Download.expandRestrictedWithLexiconService(baseTerm).toSet.mkString(","))

  def getProperty(name:String):String = properties.getProperty(name)

  def zipSelectionTo(newZip: String, selection: Set[String]):Unit = zipSelectionTo(newZip, p => selection.contains(p.getFileName.toString))

  lazy val concordances:Stream[Concordance] = documentStream.flatMap(n => KBKwic.concordance(textQuery,n()))

  lazy val f1:Int = concordances.size

  lazy val corpusSize:Int = frequencyList.values.sum

  def contextants() = Collocation.contextFrequencies(this.concordances, None)

  def addFrequencies(a:Map[String,Int], b:Map[String,Int]):Map[String,Int] =
    (a.keySet ++ b.keySet).map(x => (x, a.getOrElse(x,0) + b.getOrElse(x,0))).toMap
    //a.keySet.intersect(b.keySet).map(x => (x,a(x) + b(x))).toMap ++ a.keySet.diff(b.keySet).map(x => (x,a(x))) ++ b.keySet.diff(a.keySet).map(x => (x,b(x)))

  lazy val frequencyListOld:Map[String,Int] = // slow!!
  {
    val stukjes = QueryKB.splitStream(documentStream,100)
    val tokens = stukjes.map( s => s.flatMap(n => Tokenizer.tokenize(n().text).map(_.token)))
    val partials = tokens.par.map(ts => ts.toStream.groupBy(identity).mapValues(l => l.size))
    partials.par.reduceLeft(addFrequencies)
  }

  lazy val tf2 = documentStream.par.map(n => Tokenizer.tokenize(n().text).map(_.token).groupBy(identity).mapValues(l=>l.size)).reduceLeft(addFrequencies)

  def frequencyListFromStream(stream: Stream[()=>Node] = documentStream):scala.collection.mutable.Map[String,Int] =
  {
    val map = new java.util.concurrent.ConcurrentHashMap[String,Int]

    stream.foreach(n => {
      val tokens = Tokenizer.tokenize(n().text).map(_.token)
      tokens.foreach(s => {
        val i = map.get(s)
        if (i != null)
          map.put(s, i + 1)
        else
          map.put(s, 1)
      })
    })

    map.asScala
  }

  lazy val frequencyListy = frequencyListFromStream(documentStream)

  lazy val frequencyList =
  {
    val stukjes:Seq[Stream[()=>Node]] = QueryKB.splitStream(documentStream,8).map(_.toStream)
    val mapjes = stukjes.toList.par.map(s => frequencyListFromStream(s))
    val map0:scala.collection.mutable.Map[String,Int] = mapjes.head
    mapjes.tail.foreach(m => m.foreach(
      {
        case (k:String,v:Int) =>
          val i = map0.get(k)
          if (i.isDefined)
            map0.put(k, i.get + v)
          else
            map0.put(k, v)
      }
    )
    )
    map0
  }

  def collocations(): List[(String, Int, Int, Double)] =
  {
    val contextFrequencies = contextants()
    val enhanced = contextFrequencies
      .filter({ case (t, f) => f > 0.005 * f1 && t.matches("^[a-z]+$") })
      .map({ case (t, f) => (t, f, frequencyList(t)) })
    enhanced.map({ case (t, f, f2) => (t, f, f2, Collocation.salience(f, f1, f2, corpusSize)) })
  }
}

object KBZipje
{
  def parseBibl(b: Node):Map[String,String] = (b \\ "interpGrp")
    .map(g => (g \ "@type").text -> (g \ "interp" \ "@value").mkString("|"))
    .toMap
}

object Sample
{
  def main(args: Array[String]) =
  {
    val zip = new KBZipje(args(0))
    val selection = zip.sample(args(2).toInt, p => zip.hasLowercaseHits(p)).toSet
    println(selection)
    zip.zipSelectionTo(args(1), selection)
  }
}

object SetQuery
{
  def main(args: Array[String]):Unit = {
    val zip = new KBZipje(args(0))
    zip.setQueryTerms(args(1))
    zip.close()
  }
}

object ShowContextFromZip
{
  def main(args: Array[String]):Unit = {
    val zip = new KBZipje(args(0))
    zip.concordances.foreach(println)
  }
}

object ShowCollocationsFromZip
{
  def main(args: Array[String]):Unit = {
    val zip = new KBZipje(args(0))
    zip.collocations.sortBy(_._4).foreach(println)
  }
}

object FrequencyListFromZip
{
  def main(args: Array[String]):Unit = {
    val zip = new KBZipje(args(0))
    zip.frequencyList.toList.sortBy(_._2).foreach(println)
  }
}

object PrintAllTokens
{
  def main(args: Array[String]):Unit = {
    val zip = new KBZipje(args(0))
    val max = if (args.size > 1) args(1).toInt else Int.MaxValue
    zip.documentStream.flatMap(n => n().text.split("\\s+")).take(max).foreach(println)
  }
}

object PrintAllTokensNoPunct
{
  def main(args: Array[String]):Unit = {
    val zip = new KBZipje(args(0))
    val max = if (args.size > 1) args(1).toInt else Int.MaxValue
    zip.documentStream.flatMap(n => n().text.split("\\s+")).take(max).map(w => Tokenizer.tokenizeOne(w).token).filter(_.nonEmpty).foreach(println)
  }
}


object ZipWordCount
{
  def main(args: Array[String]):Unit = {
    val zip = new KBZipje(args(0))
    println(zip.documentStream.par.map(n => n().text.split("\\s+").size).sum)
  }
}

object printSpatial
{
  def main(args: Array[String]):Unit = {
    args.foreach(a => {
      val zip = new KBZipje(a)
      zip.metadata.map(_._2).foreach(m => if (m.contains("idno") && m.contains("spatial")) println(s"${m("idno")}\t${m("spatial")}"))
    }
    )
  }
}


