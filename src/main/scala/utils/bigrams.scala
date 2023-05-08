package utils

import java.io.FileInputStream
import java.util.zip.GZIPInputStream
import scala.collection.mutable

object bigrams {
  val trigramFrequency = new mutable.HashMap[String,Int]()
  val bigramFrequency = new mutable.HashMap[String,Int]()
  val unigramFrequency = new mutable.HashMap[String,Int]()
  var nChars:Long = 0
  var nBigrams:Long = 0
  var nTrigrams:Long = 0
  var nWords:Long  = 0
  def processWord(w: String) = {
    val wa =  w.toCharArray;
    nChars = nChars + w.length
    nWords = nWords + 1
    wa.indices.foreach(x => {
      val uni = w.substring(x, x + 1)
      unigramFrequency(uni) = 1 + unigramFrequency.getOrElse(uni, 0)

      if (x > 0) {
        val bi = w.substring(x - 1, x + 1)
        bigramFrequency(bi) = 1 + bigramFrequency.getOrElse(bi, 0)
        nBigrams = nBigrams +1
      }
      if (x > 1) {
        val tri = w.substring(x - 2, x + 1)
        trigramFrequency(tri) = 1 + trigramFrequency.getOrElse(tri, 0)
        nTrigrams = nTrigrams + 1
      }
    })}

  def processText(s: String) =  {
    //Console.err.println(s)
    s.split("\\s+").foreach(processWord)
  }

  def main(args: Array[String]): Unit = {
    val dir = new java.io.File(args.headOption.getOrElse("/mnt/Projecten/Corpora/6_Extracted_Plaintext/Portietjes/")) // new java.io.File(args(0))

    dir.listFiles.take(100).foreach(f => {
      Console.err.println(f.getName)
      val s = new GZIPInputStream(new FileInputStream(f))
      io.Source.fromInputStream(s).getLines.foreach(processText)
    })

    println(s"chars: $nChars words: $nWords" )
 
    val nnGrams = Map(1 -> nChars, 2 -> nBigrams, 3 -> nTrigrams)

    List(unigramFrequency,bigramFrequency,trigramFrequency).foreach(x => { 
      val n = x.keySet.head.length
      val p = new java.io.PrintWriter(s"/tmp/grams_$n.tsv")
      var cumul = 0.0
      x.toList.sortBy(-1 * _._2).foreach({
        case (bi,f) =>
          val percentage = 100.0 * f / nnGrams(n)
          cumul = cumul + percentage
          p.println(s"$bi\t$f\t$percentage")
        case x => println(s"What the heck $x")
      })
      println(s"$n : $cumul")
     p.close();
    }
   )
  }
}

//bigrams.main(Array())
