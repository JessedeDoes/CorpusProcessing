object x
{
  case class Triple(s: String, p: String, o:String)
  {
    lazy val isPrefix = s.toLowerCase.contains("@prefix")
    lazy val isRdfType = p.equals("<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>")
  }

  def main(args: Array[String]) =
  {
    val l = scala.io.Source.fromFile(args(0)).getLines.toStream.map(l => l.split("\\s+")).filter(c => c.size >=3)
    val pretriples = l.map(x => Triple(x(0), x(1), x.drop(2).mkString(" ").replaceAll(" \\.$", "")))

    val prefixes = pretriples.filter(_.isPrefix)

    println("Prefixen")

    prefixes.foreach(println)
 
    val triples = pretriples.filter(p => !(p.isPrefix)) 

    val resource2type = triples.filter(_.isRdfType).map(x => (x.s, x.o)).groupBy(_._1).mapValues(_.toList).mapValues(l => l.map(_._2))

    println("Domeinen")
 
    val domains = triples.flatMap(t => resource2type.getOrElse(t.s, List()).map(x => (t.p,x))).groupBy(_._1).mapValues(_.toSet).mapValues(x => x.map(_._2))

    domains.foreach({ case (p,l) => println(s"Domein van $p : ${l}") } )

    val ranges = triples.flatMap(t => resource2type.getOrElse(t.o, List()).map(x => (t.p,x))).groupBy(_._1).mapValues(_.toSet).mapValues(x => x.map(_._2))

    ranges.foreach({ case (p,l) => println(s"Bereik van $p : ${l}") } )

    val classes = triples.filter(_.isRdfType).groupBy(_.o).mapValues(_.size)

    println("Klassen")

    classes.foreach(x => println(x))

    println("Relaties")

    val relations = triples.groupBy(_.p).mapValues(_.size)
    relations.foreach(x => println(x))
  }
}
