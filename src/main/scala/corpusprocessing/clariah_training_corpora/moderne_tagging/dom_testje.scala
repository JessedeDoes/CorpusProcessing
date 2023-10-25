package corpusprocessing.clariah_training_corpora.moderne_tagging

object testje {
  case class Partition(part: String, portion: Int) {
    lazy val prefix: String = if (part == TRAIN.part && training_subsets > 1) s"$part.$portion" else part
  }

  val training_subsets = 10
  val maxje = 1000
  val p_train = 0.75
  val p_dev = 0.125
  val TRAIN: Partition = Partition("train", -1)
  val DEV: Partition = Partition("dev", -1)
  val TEST: Partition = Partition("test", -1)

  def pick() = {

  }
  def pickPartition(): Partition = {
    val r = Math.random()
    val portion = Math.floor(Math.random() * this.training_subsets).toInt
    val z = Math.min(Math.log(1 / r.toDouble) / Math.log(1.1), training_subsets+1)
    val portionLog = Math.floor(  training_subsets * (z  / training_subsets.toDouble))
    // println(s"$z $portionLog")
    val partition = if (r < p_train)
      TRAIN
    else if (r < p_train + p_dev)
      DEV
    else TEST

    val p1 = if (partition.part == TRAIN.part && this.training_subsets > 1) partition.copy(portion = z.toInt) else partition
    //Console.err.println(s"$r -> $p1 ($p_train, 388$p_dev)")
    p1
  }

  def main(args: Array[String])  = {
    val p = (0 to 10000).map(x => pickPartition()).groupBy(x => x).mapValues(_.size).toList
    p.sortBy(x => x.toString()).foreach(println)
  }
}