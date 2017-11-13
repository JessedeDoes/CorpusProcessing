package sat
import Proposition._
object Mapping {

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

  def main(args: Array[String]) =
  {
    val p  = l("mood=Part")
    val q = mapToCGN(p)
    println(s"$p => $q")
  }
}

object CGNMapping
{


  def translate(mapping: Array[Array[String]]) =
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
    // println(byLeftHand.mkString(",\n"))
    val andz = byLeftHand.map({case (k,v) => k → v}).toList
    And(andz: _*)
  }

  // lazy val mappingPropositionOld =  translate(cgnMapping)// Literal("aap")// translate(cgnMapping)
  lazy val mappingProposition = PropositionParser.parseFile("data/mapping.sorted").get

  // meeste tijd lijkt in de vertaling naar dimacs via CNF te zitten
  // dat kan worden beperkt door de mapping vooraf te compileren naar dimacs

  def time[A](p: ()=>A):A =
  {
    val t0 = System.currentTimeMillis()
    val a = p()
    val t1 = System.currentTimeMillis()
    //Console.err.println(t1-t0)
    a
  }

  def mapToTagset(p:Proposition, tagSet: Set[String]):Proposition =
  {
    // println(mappingProposition)
    val d0 = mappingProposition.dimacs.size

    val b = time( () => {
      val translation = tagSet.filter(t => {
        val lit = Literal(t)
        val extension = And(¬(lit), p)
        //val extended = And(mappingProposition, ¬(lit), p)
       // val b0 = !extended.isSatisfiable
        val b1 = !mappingProposition.isConsistentWith(extension)
        //if (b1 != b0)
         // Console.err.println("Gadsie!")
        b1
      })
      translation.map(Literal(_)).toList })

    And(b: _*)
  }

  val cgnTagSet = mappingProposition.varsIn.filter(s => s.startsWith("cgn:"))
  val udTagSet = mappingProposition.varsIn.filter(s => s.startsWith("ud:"))

  def main(args: Array[String]) =
  {
    //translate(cgnMapping)
    (0 to 1).foreach(i => println(mapToTagset(PropositionParser.parse("ud:VerbForm=Fin ∧ ud:Tense=Past").get, cgnTagSet)))
    (0 to 1).foreach(i => println(mapToTagset(PropositionParser.parse("cgn:pos=WW ∧ cgn:feat.persoon=1").get, udTagSet)))
    cgnTagSet.foreach(s => {
      val p = Literal(s)
      println(s"$s => ${mapToTagset(p, udTagSet)}")
    })
  }
}
