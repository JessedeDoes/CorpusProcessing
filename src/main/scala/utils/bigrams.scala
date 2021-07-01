package utils

import java.io.FileInputStream
import java.util.zip.GZIPInputStream
import scala.collection.mutable

object bigrams {
  val bigramFrequency = new mutable.HashMap[String,Int]()
  val unigramFrequency = new mutable.HashMap[String,Int]()
  var nChars:Long = 0
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
      }
    })}

  def processText(s: String) =  {
    //Console.err.println(s)
    s.split("\\s+").foreach(processWord)
  }

  def main(args: Array[String]): Unit = {
    val dir = new java.io.File(args(0))
    dir.listFiles.foreach(f => {
      Console.err.println(f.getName)
      val s = new GZIPInputStream(new FileInputStream(f))
      io.Source.fromInputStream(s).getLines.foreach(processText)
    })

    println(s"chars: $nChars words: $nWords" )
    List(unigramFrequency,bigramFrequency).foreach(x =>
      x.toList.sortBy(-1 * _._2).foreach({
        case (bi,f) =>
          val percentage = 100.0 * f / nChars
          println(s"$bi\t$f\t$percentage")
        case x => println(s"What the heck $x")
      }))
  }
}
