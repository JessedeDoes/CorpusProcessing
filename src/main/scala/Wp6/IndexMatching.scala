package Wp6

import java.io.File

import utils.EditDistance

import scala.xml._
import utils.PostProcessXML._

import scala.collection.{immutable, mutable}
import scala.util.matching.Regex._
import org.apache.commons.text._


object IndexMatching {
  lazy val placeIndex = Settings.part6PlaceIndexDocument
  lazy val personIndex = Settings.part6PersonIndexDocument
  lazy val shipIndex = Settings.part6ShipIndexDocument

  lazy val part6Files =  new File(Settings.outputDirectory + "split/6/").listFiles()
  lazy val part6Documents = part6Files.toStream.map(XML.loadFile)

  def extractPages(d: Elem) = {
    val leafSpul = d.descendant.filter(_.child.isEmpty)
    val pages = groupWithFirst(leafSpul, x => x.label == "pb").map(g =>
      if (g.head.label == "pb") Some((g.head \ "@n").text.toInt) -> g.map(_.text).mkString(" ") else  None -> g.text
    )
    pages.filter(_._1.nonEmpty)
  }

  //lazy val allPages = part6Documents.flatMap(extractPages).filter(_._1.isDefined).sortBy(_._1.get)

  /// allPages.foreach(println)

  def makeLines(p: Node) = {
    val lineGroups = groupWithFirst(p.child, x => x.label == "lb").map(c =>  <l>{c}</l>)
    p.asInstanceOf[Elem].copy(child = lineGroups)
  }

  case class IndexTerm(term: String, pagez: String)
  {
     val pages: List[Int] = "[0-9]+(-[0-9]+)?".r.findAllIn(pagez).toList.flatMap(s =>
     {
       if (s.contains("-")) {
         val x = s.split("-").map(_.toInt)
         (x(0) to x(1)).toList
       }  else List(s.toInt)
     })
    override def toString = s"[$term] $pages"
  }

  case class Index(index: Elem, tag: String) {

    def expand(seq: Seq[(String, String)]) = seq.foldLeft(Seq[(String, String)]()) { case (s, (t, p)) =>
      val item = (if (t.isEmpty && s.nonEmpty) s.last._1 else t) -> p
      s ++ Seq(item)
    }

    lazy val indexTerms0 = (index \\ "p").flatMap(
      p => {
        val lines = makeLines(p)
        val t_o_s = (lines \ "l").map(l => {
          val t = l.text
          val term = t.replaceAll("[.\\s,0-9-]+$", "")
          val pages = "[.\\s,0-9-]+$".r.findAllIn(t).mkString("")
          // Console.err.println(term  + " --> " + pages)
          term.trim -> pages.trim
        })
        val expanded_t_o_s = expand(t_o_s)
        expanded_t_o_s.filter(x => x._1.nonEmpty && x._2.nonEmpty)
      }
    )

    lazy val indexTerms1 = expand(indexTerms0)

    lazy val indexTerms = indexTerms1.map { case (t, i) => IndexTerm(t, i) }

    lazy val p2t = indexTerms.flatMap(t => t.pages.map(p => p -> t.term)).groupBy(_._1).mapValues(l => l.map(tag -> _._2))

    def asXML = <index tag={tag}>{indexTerms.map(t => <term pages={t.pages.toString}>{t.term}</term>)}</index>

    lazy val termIndex = indexTerms.groupBy(t => t.term.replaceAll("[,\\s].*", "").toLowerCase())

    def findMatch(s: String) = termIndex.getOrElse(s.toLowerCase, List())
  }

  lazy val index = Index(placeIndex, "location")
  lazy val pIndex = Index(personIndex, "person")
  lazy val sIndex = Index(shipIndex, "ship")

  lazy val indices = List(index,pIndex, sIndex)

  def main(args: Array[String]): Unit = {
    /// p2t.foreach(println)


    part6Files.foreach(f => {
      Console.err.println(f.getCanonicalPath)
      val d = XML.loadFile(f)
      val fOut = "/tmp/match." + f.getName
      val processedPages: scala.Seq[_root_.scala.xml.Elem] = matchIndexTerms(d)
      XML.save(fOut, <matchTest>
        {processedPages}
      </matchTest>, "UTF-8")
    })
   }

  private def matchIndexTerms(d: Elem) = {
    val allPages = extractPages(d)
    val processedPages: Seq[Elem] = allPages.map { case (Some(p), t) =>
      val terms: Seq[(String, String)] = indices.flatMap(index => index.p2t.getOrElse(p, List()))

      println(s"\n\n#### $p ### $terms ###")

      if (terms.nonEmpty) {
        val tokens = t.split("\\s+")
        val matchMap: mutable.Set[(String, String)] = new mutable.HashSet[(String, String)]()

        val matchIt = tokens.flatMap(t => {
          val tok = utils.Tokenizer.tokenizeOne(t)
          val token = tok.token.replaceAll("'s$", "")
          val distances = terms.map(term => term -> EditDistance.distance(term._2.toLowerCase.replaceAll("[\\s,].*", "").replaceAll("\\(.*?\\)",""), token.toLowerCase)).sortBy(_._2)
          val dMin = distances.head._2
          val bestMatch = distances.head._1
          val upperFirst = token.matches("^[A-Z].*")

          if (upperFirst && (dMin == 0 || dMin == 1 && token.length >= 4 || 2 >= dMin && token.length > 5)) {
            matchMap.add(bestMatch)
            Seq(<name type={bestMatch._1} norm={bestMatch._2} distance={dMin.toString}>{t}</name>, Text(" "))
          } else {
            val globalMatches = indices.flatMap(_.findMatch(token))
            if (false && upperFirst && token.length > 4 && globalMatches.nonEmpty)
              {
                Seq(<gname cands={globalMatches.toString}>{t}</gname>, Text(" "))
              }
            else Seq(Text(t + " "))
          }
        })

        val termsOnPageXML = <terms>
          {terms.map(t => <term matched={matchMap.contains(t).toString} type={t._1}>{t._2}</term>)}
        </terms>

        println(WordUtils.wrap(matchIt.mkString(" "), 60))

        <page n={p.toString}>
          <matchTerms>{termsOnPageXML}</matchTerms><content>{matchIt}</content>
        </page>
      } else <page n={p.toString}/>
    }
    processedPages
  }
}
