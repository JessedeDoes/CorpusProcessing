package corpusprocessing.kranten.oud.dubbelestukjes

import utils.alignment.{AlignmentGeneric, SimOrDiff}


object Colors {
  val KELLY_COLORS = List(
    // "0xFFB300", // Vivid Yellow
    "0x803E75", // Strong Purple
    // "0xFF6800", // Vivid Orange
    //"0xA6BDD7", // Very Light Blue
    "0xC10020", // Vivid Red
    //"0xCEA262", // Grayish Yellow
    // "0x817066", // Medium Gray
    "0x007D34", // Vivid Green
    // "0xF6768E", // Strong Purplish Pink
    "0x00538A", // Strong Blue
    "0xFF7A5C", // Strong Yellowish Pink
    "0x53377A", // Strong Violet
    // "0xFF8E00", // Vivid Orange Yellow
    "0xB32851", // Strong Purplish Red
    "0xF4C800", // Vivid Greenish Yellow
    "0x7F180D", // Strong Reddish Brown
    "0x93AA00", // Vivid Yellowish Green
    "0x593315", // Deep Yellowish Brown
    "0xF13A13", // Vivid Reddish Orange
    "0x232C16").map(_.replace("0x", "#")) // Dark Olive Green
}
case class Overlap(g: ArticleGroup, kb_article_id: String, id1: String, id2: String, evidence: Set[List[String]], allRelated: List[(String,String)]  = List()) {
  override def toString() = s"$kb_article_id:($id1 $id2) [${evidence.size}] ${evidence.toList.sortBy(x => x.toString).head.mkString(" ")}"

  lazy val art1 = g.artMap(id1)
  lazy val art2 = g.artMap(id2)

  lazy val subheader1 = g.subheaders(id1)
  lazy val subheader2 = g.subheaders(id2)

  def swap() = this.copy(id1 = this.id2, id2 = this.id1)


  def z(x: List[String]) = x.flatMap(_.split("nnnxnxnxnxnxnxn")).zipWithIndex.map({ case (y, i) => (i.toString, y) })

  def align() = {


    val a = new AlignmentGeneric(comp) // dit werkt niet...

    val z1 = z(art1.split("\\s+").toList)
    val z2 = z(art2.split("\\s+").toList)

    val chunks: Seq[SimOrDiff[(String, String)]] = a.findChunks(z1, z2)

    val lr: Seq[(Boolean, List[(String, String)], List[(String, String)], Int)] = chunks.map(
      c => {
        //Console.err.println(c)
        val left = z1.slice(c.leftStart, c.leftEnd)
        val right = z2.slice(c.rightStart, c.rightEnd)
        (c.isSimilarity, left, right, c.leftStart)
      })

    val ltext = lr.map(c => {
      val l = c._2.map(x => x._1 +":" +  x._2).mkString(" ")
      if (c._1) "<b>" + l + "</b>" else l
    })

    val rtext = lr.map(c => {
      val l = c._3.map(_._2).mkString(" ")
      if (c._1) "<b>" + l + "</b>" else l
    })

    (ltext, rtext)
  }

  lazy val align_multiple = {
    val texts = allRelated
    val z1: List[(String, String)] = z(art1.split("\\s+").toList).toList
    val marks: Seq[(Map[String, Set[Int]], Seq[String])] = texts.zipWithIndex.map({ case ((id,t),k)  =>
      val a = new AlignmentGeneric(comp)
      val z2 = z(t.split("\\s+").toList)
      val m = collection.mutable.Map[String,Set[Int]]()

      val chunks: Seq[SimOrDiff[(String, String)]] = a.findChunks(z1, z2)
      val lr: Seq[(Boolean, List[(String, String)], List[(String, String)], Int)] = chunks.map(
        c => {
          //Console.err.println(c)
          val left = z1.slice(c.leftStart, c.leftEnd)
          val right = z2.slice(c.rightStart, c.rightEnd)
          (c.isSimilarity, left, right, c.leftStart)
        })

      val ltext = lr.map(c => {
        if (c._1) c._2.foreach(x => m(x._1) = m.getOrElse(x._1, Set()) += k)
        val l = c._2.map(x => x._1 + ":" + x._2).mkString(" ")
        if (c._1) "<b>" + l + "</b>" else l
      })

      val rtext = lr.map(c => {
        val l = c._3.map(_._2).mkString(" ")
        if (c._1) "<b>" + l + "</b>" else l
      })
      (m.toMap, rtext)
    })

    val markings: Map[String, Set[Int]] = {
      val maps = marks.map(_._1)
      val keys = maps.flatMap(_.keySet)
      keys.map(k => k -> maps.flatMap(m => m.getOrElse(k, Set())).toSet)
    }.toMap

    val mValues: Set[Int] = markings.values.toSet.flatten

    val freqs: Map[Int, Int] = mValues.map(k => k -> markings.toList.count(x => x._2.contains(k))).toMap

    def colorOf(s: String) = {
      val x = markings(s).toList.minBy(freqs)
      Colors.KELLY_COLORS(x)
    }

    val averagePositions: Map[String, Double] = mValues.map(v =>{
      val positions = markings.toList.filter(x => x._2.minBy(freqs)==v).map(_._1.toInt)
      val averagePosition = positions.sum / positions.size.toDouble
      allRelated(v)._1 -> averagePosition
    }).toMap

    val partOrder = otherIds.sortBy(id => averagePositions.getOrElse(id,-1.0))

    Console.err.println(s"Multiple alignment with ${allRelated.size} texts for $kb_article_id, $id1, matches with ${marks.filter(_._1.nonEmpty).size}, " +
      s"colors: ${markings.values.toSet.flatten.map(Colors.KELLY_COLORS)} freqs=$freqs")
    val marked_text = z1.map({case (id,s)  => if (markings.contains(id)) s"<span style='color: ${colorOf(id)}'>$s</span>" else s}).mkString(" ")
    (marked_text, partOrder)
  }

  lazy val otherIds: Seq[String] = allRelated.map(_._1)
  lazy val alignment: (Seq[String], Seq[String]) = align()
  lazy val partOrder = if (allRelated.nonEmpty) align_multiple._2 else allRelated.map(_._1)
  def positionOf(id: String)  = partOrder.indices.find(i => partOrder(i) == id).getOrElse(-1)

  lazy val art1_aligned = if (allRelated.nonEmpty) align_multiple._1 else "<h2>$subheader1</h2>" + alignment._1.mkString(" ")
  lazy val art2_aligned = s"<h2>stukje ${positionOf(id2)}: $subheader2</h2>" + alignment._2.mkString(" ")
  // hier nog een difje aan toevoegen .......
}
