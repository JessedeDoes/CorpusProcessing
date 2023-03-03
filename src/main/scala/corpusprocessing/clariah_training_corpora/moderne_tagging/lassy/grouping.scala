package corpusprocessing.clariah_training_corpora.moderne_tagging.lassy

object grouping {
  def pushOptionInside[T](o: Option[(T, Int)]): (Option[T], Int) =
    o.map(x => (Some(x._1).asInstanceOf[Option[T]], x._2)).getOrElse((None, 0))

  def groupWithFirst[T](l: Seq[T], f: T => Boolean): Seq[Seq[T]] = {

    val numberedChild: Array[(T, Int)] = l.zipWithIndex.toArray


    def lastBefore(i: Int): (Option[T], Int) = {
      val countDown = (i to 0 by -1)
      val hitIndex = countDown.find(j => {
        val n = numberedChild(j)._1
        f(n)
      }).map(k => (numberedChild(k)._1, k))

      //pushOptionInside(numberedChild.filter({ case (n, j) => j <= i && f(n) }).lastOption)
      pushOptionInside(hitIndex)
    }

    val grouped = numberedChild.groupBy({ case (n, i) => lastBefore(i) })
    grouped.keySet.toList.sortBy(_._2).map(grouped).map(l => l.map(_._1).toList) // ahem, unorded...
  }

  def groupBy[T,S](l: Seq[T], f: T=>S) = {
    val numberedChild: Array[(T, Int)] = l.zipWithIndex.toArray
    val grouped = numberedChild.groupBy({ case (n, i) => f(n) }).toList.sortBy(_._2(0)._2).map({case (s,l) => (s,l.sortBy(_._2))})
    grouped
  }
}
