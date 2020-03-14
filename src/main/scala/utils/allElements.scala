package utils
import java.io.File

import scala.xml._

object allElements {

  def addMaps(m1: Map[String,Int], m2: Map[String,Int]): Map[String, Int] =
    (m1.keySet ++ m2.keySet).map(k => k -> (m1.getOrElse(k,0) + m2.getOrElse(k,0))).toMap

  def mergeCounters(c: Seq[Map[String,Int]]) = c.foldLeft(Map[String,Int]()){case (m1,m2) => addMaps(m1,m2)}

  def allElements(n: Node): Map[String,Int] = {
    addMaps(Map(n.label -> 1), mergeCounters(n.child.filter(_.isInstanceOf[Elem]).map(allElements)))
  }

  def allElementsInFile(f: java.io.File): Map[String,Int] = {
    println(f)
    if (f.isFile) allElements(XML.loadFile(f)) else if (f.isDirectory) mergeCounters(f.listFiles().map(allElementsInFile)) else Map()
  }

  def all(f1: String) = {
    val f = new java.io.File(f1)
    val tags = allElementsInFile(f)
    tags.toList.sortBy(-1 * _._2).foreach(println)
  }

  def main(args: Array[String]): Unit = {
    all(args(0))
  }
}

import allElements._
object allElemsBab {
  def main(args: Array[String]): Unit = {
    all("/mnt/Projecten/corpora/Historische_Corpora/BrievenAlsBuit/2.6CHN/")
  }
}

object allElemsONW {
  def main(args: Array[String]): Unit = {
    all("/home/jesse/workspace/data-historische-corpora/ONW/ONW-processed-metadata")
  }
}
//"/home/jesse/workspace/data-historische-corpora/gysseling/gysseling-processed-metadata"

object allElemsGys {
  def main(args: Array[String]): Unit = {
    all("/home/jesse/workspace/data-historische-corpora/gysseling/gysseling-processed-metadata")
  }
}