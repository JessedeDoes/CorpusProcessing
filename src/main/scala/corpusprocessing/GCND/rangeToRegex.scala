package corpusprocessing.GCND

object rangeToRegex {

  def padTo(x: String,n: Int) = {
    "0" * (n - x.length) + x
  }
  def testSmaller(pat: String,a: String): Unit = {
    val a1 = padTo(a, 4)
    Range(0, 2000).foreach(i => {
      val iString = i.toString
      if (iString.length <= a.length) {
        val s = padTo(iString, a.length)
        if (s <= a != s.matches(pat)) {
          println(s"Fout voor $s <= $a: $pat")
        }
      }
    })
  }

  def testGreater(pat: String, a: String): Unit = {
    val a1 = padTo(a, 4)
    Range(0, 2000).foreach(i => {
      val iString = i.toString
      if (iString.length <= a.length) {
        val s = padTo(iString, a.length)
        if ( (s >= a) != s.matches(pat)) {
          println(s"Fout voor $s >= $a: $pat")
        }
      }
    })
  }

  def smallerThan(a: String):String = if (a.length==0) "" else {
    val a0 = Integer.parseInt(a.take(1))



    val r = if (a0 > 0) range(0, a0-1) else ""
    val z = "(" + a.take(1) + "(" + smallerThan(a.drop(1)) + "))"
    val x = if (r.length > 0) s"(($r.*)|$z)" else z

    //testSmaller(x,a)
    x
  }

  def greaterThan(a: String):String = if (a.length==0) "" else {
    val a0 = Integer.parseInt(a.take(1))
    val r = if (a0 < 9) range(a0+1, 9) else ""
    val z = "(" + a.take(1) +  "(" + greaterThan(a.drop(1)) + "))"
    val x = if (r.length > 0) s"(($r.*)|$z)" else z

    // testGreater(x,a)
    x
  }

  def range(a: Int, b: Int): String  = if (a== b) s"$a" else s"[$a-$b]"
  def between(a: String, b:String):String =
  {
    if (a.length==0 && b.length==0) ""
    else if (a.length==0) smallerThan(b) else if (b.length==0) greaterThan(a)
    else if (a(0) == b(0)) a.take(1) + "(" + between(a.drop(1),b.drop(1)) + ")"
    else if (a(0) > b(0)) "⊥" else {

      val r0 = a(0).toString + "(" + greaterThan(a.drop(1)) + ")"



      val a0 = a(0).toString.toInt +1
      val b0 = b(0).toString.toInt -1

      val r1 = if (b0 >= a0) s"[${range(a0,b0)}].*"  else ""

      val r2 = b(0).toString + "(" + smallerThan(b.drop(1))  + ")"

      // println(s"($a,$b): r0: $r0; r1: $r1; r2: $r2")

      if (r1.length > 0)
        s"(($r0)|($r1)|($r2))"
      else

      s"(($r0)|($r2))"
    }
  }

  def isBetween(x: Int, y:Int, z:Int): Boolean = {
    if (x < y) z >= x && z <= y else z >= y && z <= x
  }

  def randomTest() = {
    val x = Math.floor(10000 * Math.random()).toInt + 10000
    val y = Math.floor(10000 * Math.random()).toInt + 10000
    val regex = if (x < y) {
      between(x.toString, y.toString)
    } else {
      between(y.toString,x.toString)
    }
    println(s"$regex for ($x $y)")
    Range(10000,20000).foreach(i => {
      val s = i.toString
      if (s.matches(regex) != isBetween(x,y,i)) {
        println(s"Niet goed voor ($x,$y): $i  --- $regex")
      } else {
        // println(s"OK voor ($x,$y): $i  --- $regex")
      }
    })
  }

  def test(n: Int)  = Range(0,n).foreach(i => {randomTest()})
  def main(args: Array[String])  = {
    //println(smallerThan("1231"));
    val r = greaterThan("1231");
    //println(r)
    val r1 = between("1891", "1901")
    // val r2 = "2"  + "(" + smallerThan("")  + ")"
    //println(r1)
    Range(1000, 2000).foreach(i => {
      val z = i.toString
      if (z.matches(r1)) {
        println(z);
      }
    })
    test(100)
  }
  //println(makeRegex("1231", "1232"))
}
