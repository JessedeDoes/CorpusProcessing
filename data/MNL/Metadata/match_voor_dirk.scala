val dirk = scala.io.Source.fromFile("mnl.tsv").getLines.map(l => l.split("\\t").toList).toStream
val dirkHeader = dirk.head.map(_.toLowerCase)
val dirkjes = dirk.tail.map(m => m(0) -> m)

val docs = scala.io.Source.fromFile("mnw_documents.tsv").getLines
     .map(l => l.split("\\t").toList)
     .map(l => {
        val file = l(1).replaceAll(".xml","")
        file -> l
     }).toMap
// TITEL	GENRE	GENRE	GENRE	SUBGENRE	SUBGENRE	SUBGENRE	SUBGENRE	AUTEUR	OPMERKING
// INT_ce69c331-1b00-487c-a4c0-72f0dd91e0d9        circa_instans.xml       Circa instans   genreLevel1: proza; subgenreLevel1: artesliteratuur; subgenreLevel1: geneeskunde; genre: proza; subgenre: artesliteratuur; subgenre: geneeskunde
println(dirkHeader)

dirkjes.foreach({case (f,d) =>
    val doc = docs.getOrElse(f,List()).drop(1)
    val id = doc.headOption.getOrElse("no_id")
    val l = d.mkString("\t")
    println(s"$id\t$l // $doc")
     })
