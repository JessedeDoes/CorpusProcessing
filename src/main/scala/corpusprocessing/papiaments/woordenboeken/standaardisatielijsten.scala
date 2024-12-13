package corpusprocessing.papiaments.woordenboeken

import java.io.PrintWriter

object standaardisatielijsten {

  val possiblePos = Set("sus", "ath", "ver", "adv", "ape", "pno", "pre", "kon", "art", "num", "inh", "pro")

  val posMapping = Map("sus" -> "NOU-C", "ath" -> "ADJ", "ver"-> "VRB", "adv" -> "ADV",
    "ape" -> "RES", "pno" -> "PD", "pre" -> "ADP", "kon" -> "CONJ", "art" -> "PD(type=dem,subtype=art)",
    "num" -> "NUM", "inh" -> "INT", "pro" -> "PD")

  val f = "/mnt/Projecten/Papiaments/Woordenboeken/Standaardisatiebestanden/standard.tsv"
  val fOut = "/mnt/Projecten/Papiaments/Woordenboeken/Standaardisatiebestanden/standard.parsed.tsv"


  case class ParsedWord(word: String, pos: String, islands: String, cf: String, raw_word: String, source: String) {
    def splitOU = if (word.matches(".*[ou]/[uo]$")) {
      val w1 = word.replaceAll("/.$","")
      val w2 = word.replaceAll("./", "")
      Console.err.println(s"$word --> $w1,$w2")
      List(this.copy(word=w1), this.copy(word=w2))
    } else List(this)

    def splitPoS = this.pos.split(";").map(p => this.copy(pos=p)).toList

    def split = this.splitOU.flatMap(_.splitPoS)
  }

  case class Word(raw_word: String, source: String) {
    lazy val word_1 = raw_word.replaceAll(" *;.*", "").trim
    lazy val word = word_1.replaceAll(" *[,;].*", "")

    lazy val islands =  {
      val pattern = "(\\s|\\p{P})([ABC]+)(\\s|\\p{P})".r

      // Extract substrings and convert to a list
      val substrings = pattern.findAllMatchIn(raw_word).map(_.group(2)).toList.flatMap(_.split(" *, *"))
      substrings
    }
      // if (word_1.contains(","))  word_1.replaceAll(".*[;,] *", "") else "" // nee, werkt niet


    lazy val cf = if (raw_word.matches(".*[^a-zA-z]v\\. *(\\p{L}+).*"))
    {
      val s = raw_word.replaceAll("[^a-zA-z]v\\. *(\\p{L}+(, *\\p{L}+)*)", "<$1>")
      val pattern = "<(.*?)>".r

      // Extract substrings and convert to a list
      val substrings = pattern.findAllMatchIn(s).map(_.group(1)).toList.flatMap(_.split(" *, *"))
      substrings
    } else List()

    lazy val pos: Seq[String] = if (raw_word.matches(".*[:;] *[a-z]{3}([^A-Za-z]|$).*")) {
        val pos0: Seq[String] = raw_word.replaceAll("^.*?(([;:] *([a-z]{3}(, *[a-z]{3})*) *)+)([^A-Za-z]|$).*", "$1").replaceAll("^[;:] *", "").trim.split(" *[;,:] *")
          .toList.filter(x => possiblePos.contains(x)).map(posMapping)

        if (pos0.isEmpty)  {
          val other1 =  "(^| |\\p{P})"
          val other2 =  "($| |\\p{P})"
          val r = other1 +    "(" + possiblePos.mkString("|")  + ")" + other2
          val r1 = ".*"  + r + ".*"
          Console.err.println(r1  + " " + raw_word)
          if (raw_word.matches(r1)) {
            val repechage = raw_word.replaceAll(r1, "$2")
            Console.err.println(s"Repechage: $repechage")
            List(posMapping(repechage))
          } else
            List("no_pos")
        }
        else pos0
     } else  {
      val other1 = "(^| |\\p{P})"
      val other2 = "($| |\\p{P})"
      val r = other1 + "(" + possiblePos.mkString("|") + ")" + other2
      val r1 = ".*" + r + ".*"
      //Console.err.println(r1 + " " + raw_word)
      if (raw_word.matches(r1)) {
        val repechage = raw_word.replaceAll(r1, "$2")
        Console.err.println(s"Repechage: $repechage from $raw_word")
        List(posMapping(repechage))
      } else
        List("no_pos")
    }

    lazy val parsedWord = ParsedWord(word,pos.mkString(";"),islands.mkString(";"),cf.mkString(";"), raw_word, source)
  }

  val words = io.Source.fromFile(f).getLines.map(_.split("\\t")).map({case a =>  Word(a(0),a(1))}).toList.map(_.parsedWord).flatMap(_.split)
  lazy val distinctPossen = words.map(_.pos.toSet).groupBy(identity).mapValues(_.size)
  lazy val distinctPos = words.flatMap(_.pos.split(";")).groupBy(identity).mapValues(_.size).toList.sortBy(x => -1 * x._2)


  def main(args: Array[String]) = {
    // words.foreach(w => println(w.pos))
    distinctPos.foreach(println)

    println("__________________________________________________________________")

    val p = new PrintWriter(fOut)
    words.foreach(w => p.println(s"${w.word}\t${w.islands}\t${w.pos}\t${w.cf}\t${w.source}\t${w.raw_word}"))
    p.close()
  }
}
