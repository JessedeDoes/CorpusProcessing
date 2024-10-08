package posmapping
import propositional.{And, Literal, Proposition, PropositionParser}

import scala.util.Try

case class PropositionalTagsetMapping(fileName: String, from: ()=>TagSet, to: ()=>TagSet)
{
  import Proposition._
  lazy val mappingProposition:Proposition = PropositionParser.parseFile(fileName).get
  // println(from().posTags)
  lazy val fromFeatureSet = mappingProposition.varsIn.filter(s => s.startsWith(s"${from().prefix}:"))
  lazy val toFeatureSet = mappingProposition.varsIn.filter(s => s.startsWith(s"${to().prefix}:"))

  def time[A](p: ()=>A):A =
  {
    val t0 = System.currentTimeMillis()
    val a = p()
    val t1 = System.currentTimeMillis()
    //Console.err.println(t1-t0)
    a
  }

  def mapTag(t:Tag):Tag = {
    val p1 = mapToTagset(t.proposition)
    to().fromProposition(p1)
  }

  def mapTagAsFeatures(pos: String): Set[Feature] =
  {
    val pos1 = if (pos.contains("(")) pos else pos + "()"
    val t = from().fromString(pos1)
    val p1 = mapToTagset(t.proposition)
    to().featuresFromProposition(p1)
  }

  case class Cache[A,B](f: A=>B)
  {
    val map = scala.collection.mutable.HashMap.empty[A,B]
    def apply(a: A):B = map.getOrElseUpdate(a, f(a))
  }

  def mapTag(pos: String): String =
    {
      val pos1 = if (pos.contains("(")) pos else pos + "()"
      mapTag(from().fromString(pos1)).toString
    }

  val mapTagCached = Cache(mapTagAsFeatures)

  def mapToTagset(p:Proposition, featureSet: Set[String] = toFeatureSet):Proposition =
  {
    // println(mappingProposition)
    val d0 = mappingProposition.dimacs.size

    val b = time( () => {
      val translation = featureSet.filter(t => {
        val lit = Literal(t)
        val extension = And(¬(lit), p)

        val b1 = !mappingProposition.isConsistentWith(extension)

        b1
      })
      translation.map(Literal(_)).toList })
    propositional.And(b: _*)
  }

  def main(args: Array[String]) =
  {
    fromFeatureSet.toList.sorted.foreach(s => {
      val p = Literal(s)
      println(s"$s => ${mapToTagset(p, toFeatureSet)}")
    })
  }
}


object molexTagMapping extends PropositionalTagsetMapping("data/ud2molex.mini", () => MolexTagSet, () => UDTagSet)
{
  override def main(args: Array[String]) =
  {

    if (true) io.Source.fromFile("data/molex_lemma_pos.txt").getLines.toStream.foreach(
      t => {
        //Console.err.println(t)

        println(s"$t -> ${mapTagCached(t)}")
      }
    )

    io.Source.fromFile("data/molex_wordform_pos.txt").getLines.toStream.foreach(
      t => {
        //Console.err.println(t)

          println(s"$t -> ${mapTagCached(t)}")
      }
    )
  }
}

/*
private object OldMapping {

  val basicClauses = And(
    l("pos=INTJ") → l("cgn=TSW"),
    l("pos=NOUN") →  l("cgn=ZNW"),
    l("pos=PROPN") → (l("cgn=SPEC") ∧ l("cgn=deeleigen")),
    l("pos=VERB") ∧ l("mood=Part") → (l("cgn=deelwoord") ∧ l("cgn=WW")),
    l("mood=Part") →l("pos=VERB")
  )

  def mapToCGN(p:Proposition):Proposition =
  {
    val cgnTags:Set[String] = basicClauses.varsIn.filter(s => s.startsWith("cgn"))

    val translation = cgnTags.filter(t => {
      val lit = Literal(t)
      val extended = And(basicClauses, ¬(lit), p)
      !extended.isSatisfiable
    })
    val b = translation.map(Literal(_)).toList
    And(b: _*)
  }

  def translate(mapping: Array[Array[String]]):Proposition =
  {
    val clauses:Array[Proposition] = mapping.map(a =>
    {
      val l = s"ud:${a(0)}=${a(1)}"
      val r = (2 until a.length by 2).map(i => Literal(s"cgn:${a(i)}=${a(i+1)}"))
      Implies(Literal(l), And(r: _*))
    })
    val byLeftHand = clauses.groupBy({case Implies(lhs, _) => lhs})
      .mapValues(l => l.map({case Implies(_,rhs) => rhs}))
      .mapValues(_.distinct)
      .mapValues(l => Or(l: _*))
    println(byLeftHand.mkString(",\n"))
    val andz = byLeftHand.map({case (k,v) => k → v}).toList
    And(andz: _*)
  }
  def main(args: Array[String]) =
  {
    val p  = l("mood=Part")
    val q = mapToCGN(p)
    println(s"$p => $q")
  }
}

*/