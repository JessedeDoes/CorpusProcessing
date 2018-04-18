package posmapping

object dinges {
  type streamstream = Stream[Stream[String]]


  def group[T](s: Stream[T], f: T=>Boolean): Stream[Stream[T]] =
  {
    type accum = (Stream[Stream[T]], Stream[T])
    def fold(c: accum, s:T): accum
    = {
      if (f(s))
        (c._1 ++ Stream(c._2),  Stream(s))
      else
      {
        (c._1, c._2 ++ Stream(s))
      }
    }
    val start:accum = (Stream.empty[Stream[T]], Stream.empty[T])
    val (x,y) = s.foldLeft(start)(fold)
    x ++ Stream(y)
  }

  def main(args: Array[String]): Unit = {
    val x = Stream.from(1).take(1000000)
    val xx = group[Int](x, i => i % 7==1)
    xx.foreach(x => println(x.toList))
  }
}
