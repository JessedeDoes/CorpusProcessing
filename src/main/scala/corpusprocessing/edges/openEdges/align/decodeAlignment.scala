package corpusprocessing.edges.openEdges.align

object decodeAlignment {

  def decodeAlignment(lines: Stream[String], words1: Stream[Seq[String]], words2: Stream[Seq[String]]) = {

    val alignment: Seq[Seq[(Int, Int)]] = lines.map(l => l.split("\\s+").filter(_.nonEmpty).map(s => {
      val x = s.split("-");
      (x(0).toInt, x(1).toInt)
    }).toSeq)

    val all: Seq[(Seq[(Int, Int)], (Seq[String], Seq[String]))] = alignment.zip(words1.zip(words2))

    all.flatMap({
      case (alignment, (left, right)) =>
        // Console.err.println(s"$align $left $right")
        // println(s"# $align || ${left.mkString(" ")} |||  ${right.mkString(" ")}")
        val lr: Seq[(Int, Seq[(Int, Int)])] = alignment.groupBy(_._1).toList.sortBy(_._1)

        lr.map({
          case (i, l) =>
            val w1 = left(i);
            val r = l.map(j => right(j._2))
            val w2 = r.mkString("_");
            // println(s"$w1 $w2")
            w1 -> r.toSeq
        })
    })
  }
}
