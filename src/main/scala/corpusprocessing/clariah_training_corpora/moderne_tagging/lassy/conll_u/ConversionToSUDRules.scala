package corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.conll_u
object ConversionToSUDRules extends ConversionRules {

/*
| dependentielabel | omschrijving
|---|---
| APP | appositie, bijstelling
| BODY | romp (bij complementizer))
| CMP | complementizer
| CNJ | lid van nevenschikking
| CRD | nevenschikker (als hoofd van conjunctie)
| DET | determinator
| DLINK | discourse-link
| DP | discourse-part
| HD | hoofd
| HDF | afsluitend element van circumpositie
| LD | locatief of directioneel complement
| ME | maat (duur, gewicht, . . . ) complement
| MOD | bijwoordelijke bepaling
| MWP | deel van een multi-word-unit
| NUCL | kernzin
| OBCOMP | vergelijkingscomplement
| OBJ1 | direct object, lijdend voorwerp
| OBJ2 | secundair object (meewerkend, belanghebbend, ondervindend)
| PC | voorzetselvoorwerp
| POBJ1 | voorlopig direct object
| PREDC | predicatief complement
| PREDM | bepaling van gesteldheid ‘tijdens de handeling’
| RHD | hoofd van een relatieve zin
| SAT | satelliet; aan- of uitloop
| SE | verplicht reflexief object
| SU | subject, onderwerp
| SUP | voorlopig subject
| SVP | scheidbaar deel van werkwoord
| TAG | aanhangsel, tussenvoegsel
| VC | verbaal complement
| WHD | hoofd van een vraagzin

3470 cc
  68 cc@preconj
4118 comp:aux
2115 comp:aux@pass
 685 comp@expl
32381 comp:obj
3408 comp:obl
 315 comp:obl@agent
1734 compound@prt
4289 comp:pred
2071 conj:appos
4261 conj:coord
  51 conj:coord@emb
22055 det
4807 flat
25546 mod
1466 mod@poss
1867 mod@relcl
  68 orphan
1345 parataxis
20040 punct
12289 root
14724 subj
1824 subj@pass
17872 udep
3033 unk
 125 unk@expl

 */
  val relMap: Map[String,String]  = Map(
    "su" -> "subj",
    "obj1" -> "comp:obj",
    "obj2" -> "comp:obl",
    "vc" -> "comp:aux",
    "predc" -> "comp:pred",
    "pc" -> "comp:obl",
    "mwp" -> "fixed", // maar "flat" als het een naam is.....
    "dp" -> "parataxis",
    "app" -> "conj:appos",
    "ld" -> "comp:ldmod",
    "crd" -> "cc",
    "cnj" -> "conj:coord",
    "obcomp" -> "mod:comp",
    "svp" -> "compound@prt",
    "dlink" -> "parataxis",
    "tag" -> "parataxis",
    "sat" -> "dislocated",
    "cmp" -> "comp" // nee, klopt niet.... Hangt af van rol constituent waar je head van bent

  )

  def betterRel(n: AlpinoNode): String = {

    lazy val 上: String = n.parent.get.betterRel // nee dit werkt niet, moet volgens flatlassy

    val r0: String = n.rel match {
      case _ if n.parent.isEmpty => n.rel
      case _ if (n.pos == "punct") => "punct"
      case _ if n.isWord && n.dependencyHead.isEmpty => "root"



      case "cnj" if n.dependencyHead.nonEmpty && n.dependencyHead.head.rel == "cnj" => n.rel // Pas Op deugt deze regel wel?
      case "mwp" if n.dependencyHead.nonEmpty && n.dependencyHead.head.betterRel == "mwp" => n.rel

      // first part of cooordination or multiword takes the rel of the parent node

      case "cnj" if !n.sibling.exists(x => x.rel == "cnj" && x.wordNumber < n.wordNumber) => 上
      case "mwp" if !n.sibling.exists(x => x.rel == "mwp" && x.wordNumber < n.wordNumber) => 上
      case "mwp" if n.xpos == "SPEC(deeleigen)" => "flat"

      case "hd" => 上

      case "body" =>
        if (n.parent.get.children.exists(_.rel=="cmp"))
          "comp:obj"
        else if (n.parent.get.children.exists(_.rel=="whd"))
          "comp:obj"
        else
        上 // nee niet voor "cmp"....

      // TODO check this one (and other exocentric cases)
      case "nucl" => 上

      case "rhd" => {
        if (n.index.nonEmpty && n.parent.nonEmpty) {
          val p = n.parent.get
          val corr = p.children.filter(_.rel == "body").flatMap(c => c.children.find(x => x.index == n.index  && Set("su", "obj1").contains(x.rel)))
          corr.headOption.map(_.rel + ":rhd").getOrElse("rhd")
        }  else "rhd"
      }

      case "whd" => {
        if (n.index.nonEmpty && n.parent.nonEmpty) {
          val p = n.parent.get
          val corr = p.children.filter(_.rel == "body").flatMap(c => c.children.find(x => x.index == n.index && Set("su", "obj1").contains(x.rel)))
          corr.headOption.map(_.rel + ":whd").getOrElse("whd")
        } else "whd"
      }

      case "cmp" => {
        if (n.relationsAboveMe.contains("su")) "subj"
        else if (n.relationsAboveMe.contains("vc")) "comp:obj"
        else if (n.relationsAboveMe.contains("mod")) "mod"
        else if (n.relationsAboveMe.contains("obcomp")) "mod:comp"
        else "mod:?"
      }
      // iets met whd doen!
      case _ => n.rel

    }

    val r1 = relMap.getOrElse(r0,r0)

    val r2 = if (r1=="comp:aux"  && n.dependencyHead.nonEmpty) {
      val h = n.dependencyHead.get

      if (h.lemma == "zijn" && n.xpos.contains("vd,")) {
        "comp:aux@pass"
      } else "comp:aux"
    } else r1
    r2
  }

  def findConstituentHead(n: AlpinoNode, allowLeaf: Boolean = false): Option[AlpinoNode] = {
    if (n.isWord) (if (allowLeaf) Some(n) else None) else {

      if (!n.children.exists(x => x.rel == "hd")) {
        Logje.log(s"Exocentric node: ${n.cat}:${n.rel} (${n.children.map(c => s"${c.cat}:${c.rel}").mkString(",")}) ")
      }

      def searchIn(relName: String) = n.children.find(x => x.rel == relName).flatMap(x => findConstituentHead(x, true))

      val immediateHead: Option[AlpinoNode] = n.children.find(x => x.rel == "hd" && x.isWord)

      val intermediateHead: Option[AlpinoNode] = searchIn("hd") //  n.children.filter(x => x.rel == "hd").headOption.flatMap(x => findConstituentHead(x))

      val usingCmp: Option[AlpinoNode] = searchIn("cmp")
      val usingWhd: Option[AlpinoNode] = searchIn("whd")
      val usingRhd: Option[AlpinoNode] = searchIn("rhd")
      // usingWhd.foreach(x => { println(n + " ==> "  + x) })

      val usingBody: Option[AlpinoNode] = searchIn("body")

      val usingMwp: Option[AlpinoNode] = n.children.find(_.rel == "mwp").filter(_.isWord)
      val usingCnj: Option[AlpinoNode] = searchIn("cnj") // dit werkt dus niet altijd .....
      val usingNucl: Option[AlpinoNode] = searchIn("nucl")
      val usingDp: Option[AlpinoNode] = searchIn("dp")
      // iets met whd..
      val gedoeStreepjes = n.children.find(x => x.rel == "--" && !x.isWord).flatMap(x => findConstituentHead(x))

      val r: Option[AlpinoNode] = (immediateHead.toList
        ++ intermediateHead.toList
        ++ usingCmp.toList
        ++ usingWhd.toList
        ++ usingBody.toList
        ++ usingMwp.toList
        ++ gedoeStreepjes.toList
        ++ usingCnj.toList
        ++ usingNucl.toList
        ++ usingDp.toList).headOption
      if (r == usingWhd) {
        // usingWhd.foreach(x => { println(n + " ==> "  + x) })
      }
      r
    }
  }
}