import scala.util.matching.Regex._

// honneponnig etc gemist...
// miezemuizen ...

object o extends App
{

  def vowels ="aieouy".split("").toList
  
  def consonants = "bcdfghjklmnpqrstvwxz".split("").toList

  lazy val dedupPat = "^(" + consonants.map(c => s"$c$c").mkString("|") + ")"
  def deDup(s: String) = s.replaceAll(dedupPat,"#$1").replaceAll("#.","")
  def apply(s: String, p:(String,String)):String = s.replaceAll(p._1,p._2)
  def deVoice(s: String) = List("d" -> "t", "b" -> "p", "z" -> "s", "v" -> "f").foldLeft(s)(apply)

  def maybeRedup(l0: String, r0:String) =
  {
    val l = deVoice(deDup(l0))
    val r = deVoice(deDup(r0))

    val consIni = "^[bcdfghjklmnpqrstvwxz]+".r

    // Console.err.println(s"$l|$r")

    l == r ||
    {
      val l1 = consIni.replaceAllIn(l,"")
      val r1 = consIni.replaceAllIn(r,"")
      l1.length >1 && r1.length > 1 && ((l1==r1) ||
      {
        val l1b = l1.replaceAll("e$","")
        val r1b = r1.replaceAll("ig$","")
        l1b == r1b
      }) ||
     {
       val lSkel = l.replaceAll("[aeiou]+","V")
      val rSkel = r.replaceAll("[aeiou]+","V")

      (lSkel.contains("V") && lSkel == rSkel) ||
      {
        val l1Skel = lSkel.replaceAll("V$","")
        val r1Skel = rSkel.replaceAll("(Vn|Vg)$", "")
        l1Skel == r1Skel
      }
    }}
  }

  def maybeRedup(s: String):Boolean =
  {
    val s1 = s.replaceAll("-","")
    val splits = (1 until s1.length-1).toList.map(i => s1.substring(0,i) -> s1.substring(i))
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
