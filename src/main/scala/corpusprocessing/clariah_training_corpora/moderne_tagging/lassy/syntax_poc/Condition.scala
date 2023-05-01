package corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.syntax_poc

trait ICondition {
  def name: String
  def filter: Seq[IHeadedSpan] => Boolean
  override def toString() = name
}

case class BasicCondition(name: String, filter: Seq[IHeadedSpan] => Boolean) extends ICondition

case class ChildOrderCondition(name: String) extends ICondition {

  val clauses: Seq[Seq[IHeadedSpan] => Boolean] = name.trim.split("\\s*&\\s*").map(c => {
    val name = c
    val operator = c.replaceAll("[^<>]","")
    val leftright = c.split("\\s*[<>]\\s*")

    val left = leftright(0).toInt
    val right = leftright(1).toInt

    val filter : Seq[IHeadedSpan] => Boolean = operator match {
      case ">" =>  s => s.size > left && s.size > right && s(left).start > s(right).start
      case "<" =>  s => s.size > left && s.size > right && s(left).start > s(right).start
    }
    filter
  })

  val filter = s => clauses.forall(_(s))
}

case class CaptureOrderCondition(condition: String) extends ICondition {

  val name = s"$condition"
  val clauses: Seq[Seq[IHeadedSpan] => Boolean] = condition.trim.split("\\s*&\\s*").map(c => {
    val name = c
    val operator = c.replaceAll("[^<>]","")
    val leftright = c.split("\\s*[<>]\\s*")

    val left = leftright(0)
    val right = leftright(1)

    val filter : Seq[IHeadedSpan] => Boolean =  {
      operator match {
      case "<" =>  s =>
         val h = s.head
         val lo = h.captures.find(_._1==left)
         val ro = h.captures.find(_._1==right)
         lo.flatMap(l => ro.map(r => l._2 < r._2)).getOrElse(false)

      case ">" =>  s =>
        val h = s.head
        val lo = h.captures.find(_._1==left)
        val ro = h.captures.find(_._1==right)
        //Console.err.println(s"l=$lo, r=$ro $left, $right")
        lo.flatMap(l => ro.map(r => l._2 > r._2)).getOrElse(false)
    }}
    filter
  })

  val filter = s => clauses.forall(_(s))
}

case class ConditionAnd(c1: ICondition, c2:ICondition) extends ICondition {
  val filter: Seq[IHeadedSpan] => Boolean = s => c1.filter(s) && c2.filter(s)
  def name: String  = s"{${c1.name}} & {${c2.name}} "
}

object Condition {
  def isUnique(spans: Seq[IHeadedSpan]): Boolean = spans.toSet.size == spans.size

  val unique: BasicCondition = BasicCondition("unique", isUnique)
  val sameHead: BasicCondition = BasicCondition("sameHead", s => s.map(_.head).toSet.size == 1)
  val trivial: BasicCondition = BasicCondition("trivial", s => true)
  val twee_voor_een: BasicCondition = BasicCondition("twee_voor_een", s=> {
    // println(s.map(_.toString))
    s.size >= 2 && s(1).start < s(0).start}
  )

  val defaultCondition: ConditionAnd =  ConditionAnd(unique, sameHead)
  val defaultAndersom: ConditionAnd = ConditionAnd(defaultCondition, ChildOrderCondition("0>1"))
}
