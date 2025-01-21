package corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.conll_u.dependencyconversion

import corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.conll_u.Logje.log
import corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.conll_u.{AlpinoNode, ConversionRules}



object ConversionToFlatLassyRules extends ConversionRules {
  def betterRel(n: AlpinoNode): String = {

    lazy val 上: String = n.parent.get.betterRel

    val r0: String = n.rel match {
      case _ if n.parent.isEmpty => n.rel
      case _ if (n.pos == "punct") => "punct"
      case _ if n.isWord && n.dependencyHead.isEmpty => "root"



      case "cnj" if n.dependencyHead.nonEmpty && n.dependencyHead.head.rel == "cnj" => n.rel // Pas Op deugt deze regel wel?
      case "cnj" if !n.sibling.exists(x => x.rel == "cnj" && x.wordNumber < n.wordNumber) => 上

      case "mwp" if n.dependencyHead.nonEmpty && n.dependencyHead.head.betterRel == "mwp" => n.rel

      // first part of cooordination or multiword takes the rel of the parent node

      
      case "mwp" if !n.sibling.exists(x => x.rel == "mwp" && x.wordNumber < n.wordNumber) => 上

      case "hd" => 上
      case "body" => if (n.parent.get.children.exists(_.rel == "cmp"))
        "body"
      else // nee, dit wil je helemaal niet in geval van een "cmp" ernaast
        上

      // TODO check this one (and other exocentric cases)
      case "nucl" => 上
      case "cmp" if (n.dependencyHead.nonEmpty && n.parent.nonEmpty) => {
        val h = n.dependencyHead.get
        val p = n.parent.get
        if (p.rel == "obcomp") "obcomp" else "cmp"
        /*
        if (n.word == "dan") println(n  + "-head->" + h)
        val betterRelsAbove =  { val z = h.constituentsIAmTheHeadOf.map(_.rel)
          if (n.word == "dan") println("Above me:" + z)
          z.filter(x => x=="obcomp") }
        betterRelsAbove.headOption.getOrElse("cmp")
         */
      }
      case _ => n.rel
    }

    r0
  }

  def findConstituentHead(n: AlpinoNode, allowLeaf: Boolean = false): Option[AlpinoNode] = {
    if (n.isWord) (if (allowLeaf) Some(n) else None) else {

      if (!n.children.exists(x => x.rel == "hd")) {
        log(s"Exocentric node: ${n.cat}:${n.rel} (${n.children.map(c => s"${c.cat}:${c.rel}").mkString(",")}) ")
      }

      def searchIn(relName: String) = n.children.find(x => x.rel == relName).flatMap(x => findConstituentHead(x, true)).toList

      val immediateHead: Option[AlpinoNode] = n.children.find(x => x.rel == "hd" && x.isWord)

      val intermediateHead = searchIn("hd") //  n.children.filter(x => x.rel == "hd").headOption.flatMap(x => findConstituentHead(x))

      val usingCmp = searchIn("cmp")

      val usingWhd = searchIn("whd")
      val usingRhd = searchIn("rhd")
      val usingBody= searchIn("body")

      val usingMwp = n.children.find(_.rel == "mwp").filter(_.isWord).toList
      val usingCnj = searchIn("cnj") // dit werkt dus niet altijd .....
      val usingNucl = searchIn("nucl")
      val usingDp = searchIn("dp")

      val gedoeStreepjes = n.children.find(x => x.rel == "--" && !x.isWord).flatMap(x => findConstituentHead(x)).toList

      (immediateHead.toList
        ++ intermediateHead
        ++ usingCmp
        ++ usingWhd
        ++ usingRhd
        ++ usingBody
        ++ usingMwp
        ++ gedoeStreepjes
        ++ usingCnj
        ++ usingNucl
        ++ usingDp).headOption
    }
  }
}

//case "cnj" if parent.exists(_.cat=="conj" && !(parent.get.children.exists(_.wordNumber < this.wordNumber))) => s"Tja!: ${parent.get.children.map(_.rel).mkString("|")}"
