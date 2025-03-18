package corpusprocessing.gentse_spelen

import corpusprocessing.brievenalsbuit.bab.getId

import scala.xml._
import utils.PostProcessXML

import scala.collection.mutable

case class TokenGroups(d: Elem) {
  lazy val wordElements = (d \\  "w").toList

  def markGroups() = {
    val groups: Map[Int, Group] = findGroups()
    val id2groupid: Map[String, Int] = groups.values.toList.flatMap(g => g.members.map(s => s -> g.id)).toMap
    val d1 = PostProcessXML.updateElement4(d, _.label=="w", w => {
        val id = (w \ "@id").text
        if (id2groupid.contains(id)) {
          val gid = id2groupid(id).toString
          val z = w.copy(child = w.child ++ <join n={gid}/>)
          println(z)
          z
        } else w
    })
    d1.asInstanceOf[Elem]
  }

  def transitiveClosure[T](classes: Set[Set[T]]): Set[Set[T]] = {
    def mergeSets(equivClasses: Set[Set[T]]): Set[Set[T]] = {
      val merged = equivClasses.foldLeft(Set.empty[Set[T]]) { (acc, set) =>
        val (overlapping, disjoint) = acc.partition(_.exists(set.contains))
        disjoint + (overlapping.flatten ++ set)
      }
      if (merged == equivClasses) merged else mergeSets(merged)
    }
    mergeSets(classes)
  }

  case class Group(members: Set[String], id: Int)

  val lpfreqs = wordElements.map(w =>  (w\ "@lemma").text + "__" + (w \ "@ctag").text).groupBy(identity).mapValues(_.size)
  def jofel(s: String) = {


    val ls = s.split("__")
    if (ls.size < 2)
      false else {
      val (lemma, pos) = (ls(0), ls(1))
       lpfreqs(s) < 10  && pos != "FOREIGN"
    }
  }

  def findGroups()  = {

    val primaryGroups: Set[Set[String]] = wordElements.sliding(9).flatMap(l => {
      val groups: Map[String, List[Node]] = l.groupBy(w =>  (w\ "@lemma").text + "__" + (w \ "@ctag").text)
      val z: Iterable[Set[Node]] = groups.filter(x => jofel(x._1)).values.filter(_.size > 1).map(_.toSet)
      z.map(s => s.map( w => (w \ "@id").text))
    }).toSet
    val equivgroups = transitiveClosure(primaryGroups).zipWithIndex.map({case (s,i) => Group(s,i)})

    val groups: Map[Int, Group] = equivgroups.map(g => g.id -> g).toMap
    groups.foreach(println)

    groups
  }
}
