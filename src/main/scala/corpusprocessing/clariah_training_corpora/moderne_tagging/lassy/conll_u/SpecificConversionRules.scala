package corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.conll_u

import corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.conll_u.Logje.log

object SpecificConversionRules {
  def betterRel(n: AlpinoNode): String = {

    lazy val up: String = n.parent.get.betterRel

    val r0: String = n.rel match {
      case _ if n.parent.isEmpty => n.rel
      case _ if (n.pos == "punct") => "punct"
      case _ if n.isWord && n.dependencyHead.isEmpty => "root"

      // nonfirst part of cooordination or multiword keeps its rel

      case "cnj" if n.dependencyHead.nonEmpty && n.dependencyHead.head.rel == "cnj" => n.rel // Pas Op deugt deze regel wel?
      case "mwp" if n.dependencyHead.nonEmpty && n.dependencyHead.head.betterRel == "mwp" => n.rel

      case "cnj" if !n.sibling.exists(x => x.rel == "cnj" && x.wordNumber < n.wordNumber) => up
      //case "cnj" if parent.exists(_.cat=="conj" && !(parent.get.children.exists(_.wordNumber < this.wordNumber))) => s"Tja!: ${parent.get.children.map(_.rel).mkString("|")}"
      case "mwp" if !n.sibling.exists(x => x.rel == "mwp" && x.wordNumber < n.wordNumber) => up

      case "hd" => up
      case "body" => up


      case "nucl" => up
      case _ => n.rel
    }
    // if (dependencyHead.isEmpty || r0=="0") "root" else if (pos == "punct") "punct" else r0
    r0
  }

  def findConstituentHead(n: AlpinoNode, allowLeaf: Boolean = false): Option[AlpinoNode] = {
    if (n.isWord) (if (allowLeaf) Some(n) else None) else {

      if (!n.children.exists(x => x.rel == "hd")) {
        log(s"Exocentric node: ${n.cat}:${n.rel} (${n.children.map(c => s"${c.cat}:${c.rel}").mkString(",")}) ")
      }

      def searchIn(relName: String) = n.children.find(x => x.rel == relName).flatMap(x => findConstituentHead(x, true))

      val immediateHead: Option[AlpinoNode] = n.children.find(x => x.rel == "hd" && x.isWord)

      val intermediateHead: Option[AlpinoNode] = searchIn("hd") //  n.children.filter(x => x.rel == "hd").headOption.flatMap(x => findConstituentHead(x))

      val usingCmp: Option[AlpinoNode] = searchIn("cmp")
      val usingBody: Option[AlpinoNode] = searchIn("body")

      val usingMwp: Option[AlpinoNode] = n.children.find(_.rel == "mwp").filter(_.isWord)
      val usingCnj: Option[AlpinoNode] = searchIn("cnj") // dit werkt dus niet altijd .....
      val usingNucl: Option[AlpinoNode] = searchIn("nucl")
      val usingDp: Option[AlpinoNode] = searchIn("dp")

      val gedoeStreepjes = n.children.find(x => x.rel == "--" && !x.isWord).flatMap(x => findConstituentHead(x))

      (immediateHead.toList
        ++ intermediateHead.toList
        ++ usingCmp.toList
        ++ usingBody.toList
        ++ usingMwp.toList
        ++ gedoeStreepjes.toList
        ++ usingCnj.toList
        ++ usingNucl.toList
        ++ usingDp.toList).headOption
    }
  }
}
