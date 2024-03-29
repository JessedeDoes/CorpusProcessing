package corpusprocessing.delpher.serpens

/**
  * Created by does on 7/20/17.
  */

// http://nederlandsesoorten.nl/nsr/concept/000464756207	Bergbehangersbij	Megachile alpicola Alfken, 1924	[infraspecificRank:null, phylum:Arthropoda, order:Hymenoptera, kingdom:Animalia, family:Apidae, className:Insecta, subgenus:null, genus:Megachile, superFamily:null, infraspecificEpithet:null, specificEpithet:alpicola]
import corpusprocessing.delpher.QueryKB
import utils._
import corpusprocessing.delpher._
import corpusprocessing.delpher.SRU._

object BeestenZoeker
{
  case class Beest(id: String, name: String, latin: String, taxonomy: String)
  {
    lazy val query = Phrase(name.split("\\s+").map(SingleTerm(_)):_*)
    lazy val lastpart = { val parts = name.split("\\s+"); parts(parts.size-1) }
    lazy val lastpartQuery = if (lastpart == name) None else Some(SingleTerm(lastpart))
    def getQuery():SRUQuery = if (name.startsWith("!")) lastpartQuery.get else query
  }

  def doLine(line: String): Beest =
  {
    val cols = line.split("\\t", -1)
    Beest(cols(0),cols(1), cols(2), cols(3))
  }

  val onBeesten = Set("aderen", "foret", "raar", "wessel", "wuif")

  def onBeest(s: String):Boolean = onBeesten.contains(s)

  def sums(s : Seq[Int], acc : Int = 0) : Stream[Int] =
    if (s.isEmpty) Stream.empty else
    Stream.cons(s.head + acc, sums(s.tail, s.head + acc))

  def zoekop(fileName: String): Unit =
  {
    val beesten = scala.io.Source.fromFile(fileName).getLines.map(doLine).filter(_.taxonomy.contains("Chordata")).toStream
    Console.err.println(beesten.take(5).toList)
    val aantallenCompleteNaam = beesten.par.map(b => (b, QueryKB.getNumberOfResults(b.query)))

    val aantallenLaatsteDeel = beesten.filter(_.lastpartQuery != None).map(b => b.copy(name="!" + b.name)).par.map(b => (b, QueryKB.getNumberOfResults(b.lastpartQuery.get)))


    val nonZero = aantallenCompleteNaam.filter(_._2 > 0).toList
    val aldNonZero = aantallenLaatsteDeel.filter(_._2 > 0).toList.groupBy(_._1.lastpart).mapValues(_.head).values

    nonZero.foreach( {case (b,a) => println( List(b.id, b.name, a).mkString("\t")) } )


    aldNonZero.foreach( {case (b,a) => println( List(b.id, b.name + "*", a).mkString("\t")) } )

    val all = (nonZero ++ aldNonZero).sortBy(_._2)
    val withSums = all.zip(sums(all.map(_._2)))


    withSums.foreach(println)

    // eerst kleiner dan 100 gedaan

    val kleineBeesten = all.filter(x => x._2 >= 1000 && x._2 < 2000)

    val kleineBeestQueries = kleineBeesten.map(_._1.getQuery()).toSet

    //kleineBeestQueries.foreach(q => QueryKB.downloadPar(q))
    // val sums = (0 to all.size).map(all.take(_).map(_._2).sum)
  }

  def main(args: Array[String]):Unit =
  {
    zoekop(args(0))
  }
}
