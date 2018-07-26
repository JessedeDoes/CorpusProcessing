import scala.util.matching.Regex._

// honneponnig etc gemist...
// miezemuizen ...

object o extends App
{

  val vowels ="aieouy".split("").toList
  
  val consonants = "bcdfghjklmnpqrstvwxz".split("").toList

  def maybeRedup(l: String, r:String) =
  {
    val consIni = "^[bcdfghjklmnpqrstvwxz]+".r

    // Console.err.println(s"$l|$r")

    l == r || 
    {
      val l1 = consIni.replaceAllIn(l,"")
      val r1 = consIni.replaceAllIn(r,"")
      l1.length >1 && r1.length > 1 && (l1==r1)
    } ||
    {
      val lSkel = l.replaceAll("[aeiou]+","V")
      val rSkel = r.replaceAll("[aeiou]+","V")

      lSkel.contains("V") && lSkel == rSkel
    }
  }

  def maybeRedup(s: String):Boolean =
  {
    val splits = (1 until s.length-1).toList.map(i => s.substring(0,i) -> s.substring(i))
      .filter({ case (x,y) => maybeRedup(x,y) })
    if (splits.nonEmpty) println(splits)
    splits.nonEmpty
  }


   override def main(args: Array[String]):Unit = {

   Console.err.println(args(0))

   val  words = io.Source.fromFile(args(0)).getLines.toStream
   words.foreach(s => maybeRedup(s))
  }
}
