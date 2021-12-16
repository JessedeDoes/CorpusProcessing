package corpusprocessing.kranten.oud

import corpusprocessing.kranten.oud.Settings.{articles, krantendb}
import database.DatabaseUtilities.Select
import utils.Tokenizer

import java.io.PrintWriter

// Louis van Baden
//  Ceurvorst van Saxen
// Marckgraef van Bareith
case class City(land: String, plaats: String, spelling: String, id:Int) {
  override def toString: String = s"{$plaats/$land}"
}

case class Node(spelling: String, next: Map[String,Node] = Map.empty, cities: Set[City] = Set.empty) {
  lazy val unique_cities = cities.groupBy(x => (x.land, x.plaats)).values.map(_.head).toSet
}

case class Match(a: Article, hit: Seq[(Int,Node)], tokens: Seq[Tokenizer.Token]) {
  def cities = hit.last._2.unique_cities
  def matched_form = hit.tail.map{ case (i,n) => tokens(i).toString }.mkString(" ")
  def subMatchOf(m: Match) = hit.map(_._1).toSet.subsetOf(m.hit.map(_._1).toSet)
  def properSubmatchOf(m: Match) = subMatchOf(m) && hit.size < m.hit.size
}

object Plaatsnamen {

  val cities = krantendb.slurp(
    Select(
       r => City(r.getString("land"), r.getString("plaats"), r.getString("spelling"), r.getInt("id")), "cities_rob where (not fishy) or changed"))

  def oneToNode(c: City) : Node = {
    val tokens = Tokenizer.tokenize(c.spelling).reverse
    val n0 = Node(tokens(0).token.toLowerCase(), cities = Set(c))
    tokens.tail.foldLeft(n0){case (n,t) => Node(t.token.toLowerCase, Map(n.spelling.toLowerCase -> n))}
  }


  def add(m: Map[String, Node], n: Node): Map[String,Node] = {
    if (m.contains(n.spelling)) {
      // println("adding: "  + n)
      val n0 = m(n.spelling)
      val mNew = n.next.foldLeft(n0.next){case (m1,(k,n)) => add(m1,n)}
      val nNew = Node(n.spelling, mNew, cities = n0.cities ++ n.cities)
      (m - n.spelling) + (nNew.spelling -> nNew)
    } else m + (n.spelling -> n)
  }

  def addCity(m: Map[String,Node], c: City): Map[String,Node] = add(m, oneToNode(c))

  lazy val cityTree: Map[String,Node] = cities.foldLeft(Map.empty[String,Node]){case (m,c ) => addCity(m,c)}
  lazy val cityMap = cityTree
  lazy val nodeZero = Node("0", cityTree)

  def findCity(a : Article)  = {
    val tokens = Tokenizer.tokenize(a.subheader)

    def extend(l: Seq[(Int,Node)], i: Int): Set[Seq[(Int, Node)]] = {
      if (i >= tokens.size) Set.empty else {
        val t = tokens(i).token.toLowerCase
        val (i0,cur) = l.last
        if (cur.next.contains(t)) {
          val cont = l :+ i -> cur.next(t)
          Set(l, cont) ++ extend(cont, i + 1)
        } else
          Set(l)
      }
    }

    val hits: Seq[Seq[(Int,Node)]] = tokens.indices.flatMap(i => extend(Seq(i -> nodeZero), i)).filter(l => l.nonEmpty && l.tail.nonEmpty && l.last._2.cities.nonEmpty)
    val matches = hits.map(Match(a, _, tokens))

    val maximal_matches = matches.filter(m => !matches.exists(m1 => m.properSubmatchOf(m1)))
    
    val cities = hits.flatMap(_.last._2.unique_cities).toSet

    val unique_cities = cities.groupBy(x => (x.land, x.plaats)).values.map(_.head).toSet

    unique_cities
    maximal_matches
  }

  def findCities() =  {
    val matches: Seq[(Article, Seq[Match])] = articles.map(a => a -> findCity(a)).toStream

    val all = new PrintWriter("/tmp/all.txt")

    val citiesFound: Seq[(Article, Set[City])] = matches.map{case (a,matches) => (a,matches.flatMap(_.cities).toSet)}

    citiesFound.foreach{case (a, c) =>
      //all.println(s"${c.exists(_.plaats==a.city)}\t${a.record_id}\t${a.header}\t${a.subheader}\t${a.city}\t${c.mkString(";")}")
    }

    matches.foreach{case (a,ms) =>
      val c = ms.flatMap(_.cities).toSet
      val asFound: Set[String] = (ms.map(_.matched_form).toSet)//.map(s => s"<$s>")// .mkString("; ")
      val asFoundP = asFound.map(s => s"<$s>").mkString("; ")
      all.println(s"${c.exists(_.plaats==a.city)}\t${a.tekstsoort}\t${a.record_id}\t${a.header}\t${a.subheader}\t${a.city}\t${c.mkString(";")}\t$asFoundP")
    }
    all.close()

    val N = citiesFound.size
    val nonEmpty = citiesFound.count(_._2.nonEmpty)
    val empty = citiesFound.count(x => x._2.isEmpty && !x._1.tekstsoort.matches(".*(verten|streep).*"))
    val changed: Seq[(Article, Set[City])] = citiesFound.filter{case (a,  unique_cities)
      => {
        val matching = unique_cities.exists(_.plaats == a.city)
        !matching && cities.nonEmpty
     }}

    val nChanged = changed.size

    val changes: Seq[((String, Set[City]), Int)] = changed.groupBy{case (a,c) => a.city -> c}.mapValues(_.size).toList.sortBy(-1 * _._2)

    println(s"N=$N, nonEmpty=$nonEmpty, changed=$nChanged, empty (geen advertentie)=$empty")

    // changes.take(100).foreach(println)

    val pw = new PrintWriter("/tmp/plaatsmatches.txt")

    val samples = citiesFound.groupBy(_._2).toList.sortBy(-1 * _._2.size).map{case (c,l) =>
      val N = l.size
      val l1 = l.groupBy(_._1.normalized_subheader).values.map(_.head)
      val sample = scala.util.Random.shuffle(l1).take(25)
      pw.println(s"\n### $c ### $N ###")
      sample.foreach{case (a,c) => pw.println(s"\t\t${a.header} // ${a.subheader} // ${a.normalized_subheader}")}
    }
    pw.close()
    // if () println(a.subheader + " ## <" + a.city + "> ---> " + unique_cities.mkString("; "))}
  }

  def printNode(n: Node, indent: Int): Unit =
    {
      (0 to indent).foreach(x => print(" "))
      println(n.spelling + " -> " + n.cities.headOption)
      n.next.values.foreach(printNode(_, indent+2))
    }

  def main(args: Array[String]): Unit = {
     // cityTree.foreach{case (k,n) => printNode(n,0)}
    // println(cityTree)
    findCities()
  }
}


/*
Plaatsnamen

Dag Jesse,

De plaatsnamenklus is in een nieuw stadium aanbeland. Alle automatisch toegekende plaatsnamen zijn handmatig gecontroleerd en waar nodig verbeterd.

Wat moet je weten bij de verdere behandeling?

•	Als de kolom fishy is aangevinkt kan dat twee dingen betekenen:

o	er zijn geen attestaties gekoppeld aan dit id en deze regels kunnen daarom weggegooid worden (tenzij ze een andere functie vervullen).
Let op: aan deze regels is verder geen aandacht geschonken (aan de relatie tussen land en plaats moet dan ook geen aandacht worden geschonken; die kan namelijk onjuist zijn).

o	er was verschil in spelling tussen plaats en spelling, omdat er sprake was van een drukfout, een transcriptiefout of een onjuiste automatische toekenning van plaats.
Bij elk van deze gevallen is aangegeven of het gaat om een drukfout of een transcriptiefout.
Een enkele maal is niet zeker welke plaatsnaam bedoeld is. Dan is daar een opmerking bij gemaakt.

•	Lang niet alle attestaties van een bepaalde spelling hebben een header.
•	Sommige attestaties hebben een verkeerde header omdat ze als nagekomen bericht zijn opgenomen. In de kranten zelf is dat eenvoudig te zien door een streep, maar deze berichten zijn nu opgenomen onder de laatstgenoemde header.
•	Sommige id’s zijn identiek. Dit levert dubbele attestaties op. Deze zouden het best verwijderd kunnen worden.

 */