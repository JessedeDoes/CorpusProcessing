object EditDistance {
  import scala.math._

    def minimum(i1: Int, i2: Int, i3: Int)=min(min(i1, i2), i3)

    def distance(s1:String, s2:String)=
    {
      val dist=Array.tabulate(s2.length+1, s1.length+1){(j,i)=>if(j==0) i else if (i==0) j else 0}

      for(j<-1 to s2.length; i<-1 to s1.length)
        dist(j)(i)=if(s2(j-1)==s1(i-1)) dist(j-1)(i-1)
        else minimum(dist(j-1)(i)+1, dist(j)(i-1)+1, dist(j-1)(i-1)+1)

      dist(s2.length)(s1.length)
    }
    def ciDistance(s1:String, s2:String)= distance(s1.toLowerCase, s2.toLowerCase)

    def lengthWeightedDistance(s1:String, s2:String):Double =
      2*ciDistance(s1.toLowerCase, s2.toLowerCase) / (s1.length + s2.length).asInstanceOf[Double]

    def printDistance(s1:String, s2:String)=println("%s -> %s : %d".format(s1, s2, distance(s1, s2)))
}


import scala.io.Source._

val splitsingen = fromFile("ww.lemmata.txt").getLines.map(l => l.split("\\|")).map(l => l.mkString("") -> l.toList).toMap

//println(splitsingen)

fromFile("ww.uniq.log").getLines.toList.map(l => l.split("\\s*/\\s*")).map(l =>
{
  val (word,lemma,tag) =  (l(0), l(1), l(2))
  val s = splitsingen.getOrElse(lemma,List(lemma))
  val p = s.permutations
  
  val verbalPart = s.last


  val best = p.minBy(p => EditDistance.distance(word.replaceAll("\\s+",""), p.mkString  )) 
  val placeOfVerbalPart = best.indexOf(verbalPart) 
  val wordParts = word.split("\\s+").toList
  val verbalWordPart = if (wordParts.size == s.size) wordParts(placeOfVerbalPart) else wordParts.minBy(p => EditDistance.ciDistance(verbalPart,p))

  if (s.size > 1)  println(s"$word\t${best.mkString("|")}\t$verbalWordPart\t$verbalPart")
})
