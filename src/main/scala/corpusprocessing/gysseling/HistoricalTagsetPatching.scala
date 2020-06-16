package corpusprocessing.gysseling

import posmapping.TagsetDiachroonNederlands

import scala.util.{Failure, Success, Try}

object HistoricalTagsetPatching {

  val bw2aa = false

  val addPartTypes = false

  val advSubtypeMap = Map("demonstrative" -> "dem", "general" -> "gen", "indefinite" -> "indef", "interrogative" -> "inter", "negative" -> "neg", "relative" -> "rel")

  val gysParticles: Map[String, String] = io.Source.fromFile("data/Gys/separates.corr.txt").getLines.map(l => l.split("\\t")).map(l => l(1) -> l(2) ).toMap

  val gysParticleLemmata: Map[String, String] = io.Source.fromFile("data/Gys/separates.corr.txt").getLines
    .map(l => l.split("\\t"))
    .map(l => {
      val lem = l(4)
      val deel = l(2)
      val lemPatched = if (deel.contains("ww") || deel.contains("bw")) lem.replaceAll("\\|","") else lem.replaceAll("\\|","-")
      l(1) -> lemPatched }  )
    .toMap

  def gysFixPartLemma(n: String):Option[String] =
  {
    gysParticleLemmata.get(n)
  }

  def getFeature(tag: String, name: String) = {
    val r = s"$name=([^,()]*)".r
    r.findFirstMatchIn(tag).map(_.group(1))
  }

  def patchPoSMistakes(m: String, p:String, w: String, lemma: String): String =
  {
    // println(s"$m\t$p")
    val infl = getFeature(p,"infl").getOrElse("other")

    val p1 = if (false && p.startsWith("NUM") && p.contains("indef")) {
      p.replaceAll("NUM", "PD").replaceAll("indefinite", "indef")
    }

    else if (false && p.startsWith("ADP")) {
      p.replaceAll("type=gen(,?)","")
    }

    else if (false && p.startsWith("ADJ")) {
      val base =  if (bw2aa) p.replaceAll("ADJ","AA").replaceAll("\\)",",position=pred|prenom|postnom)") else p
      base.replaceAll("number=","NA=") // Vervelend!!!
    }


    else if (p.startsWith("VRB") && p.contains("=part")) {
      val tense = if (w.toLowerCase().endsWith("nde")) "pres" else "past"
      p.replaceAll("finiteness=part","finiteness=" + tense + "part").replaceAll("number=","NA=")
    }
    // else if (m.startsWith("25") && p.contains("=finite")) p.replaceAll("=finite", "=inf")
    else p

    val p2 = p1.replaceAll("inflection=","infl=").replaceAll("=main","=mai")
      .replaceAll("=finite","=fin")
      .replaceAll("=-","=other")
      .replaceAll("=(card|ord)inal","=$1")
      .replaceAll("=present", "=pres")
      .replaceAll("=imperative", "=imp")
      .replaceAll("=adverbial", "=adv")
      .replaceAll("=coord","=coor")
      .replaceAll("type=interjection(,?)","")
      .replaceAll("=general","=gen")
      .replaceAll("recp","recip")
      .replaceAll("=negative","=neg")

    val integratedTag = Try(TagsetDiachroonNederlands.integratedTag(p2)) match {
      case Success(t) =>
      case Failure(exception) => Console.err.println(s"exception for $p2: $exception")
    }

    // println(s"$m\t$p\t$p2")
    p2
  }

  def getTaggingFromMorfcodes(tagMapping: Map[String,String], morfcodes: List[String], morfcodes_original: List[String],
                              w: String, lemmata: Seq[String], n: String, isPartOfSomethingGreater: Boolean) = {
    val posFromMapping = morfcodes.zip(lemmata).map({case (m,l) =>
      val m1 = if (l.toLowerCase.equals("beide") && m.equals("321")) "301" else m
      tagMapping.getOrElse(m1, s"U$m1")})

    val deelSoort = gysParticles.get(n)

    val patchedPos = morfcodes.zip(morfcodes_original).zip(posFromMapping).zip(lemmata).map{
      case (( (a,o),b),l) =>
        val t = HistoricalTagsetPatching.patchPoSMistakes(a,b,w,l)
        if (!addPartTypes || deelSoort.isEmpty || !o.contains("{"))
        t else {
           val soort =  deelSoort.get
           t.replaceAll( "\\)", ",wordpart=part,partType=" + soort + ")" ).replaceAll("\\(,", "(")
        }
    }
    patchedPos
    // patchedPos.mkString("+")
  }
}

/*
 // deprecate this one, it is a mess.....
  // maar wel even kijken naar 285 en 655, wat daar ook alweer mee was in CRM

  def morfcode2tag(tagMapping: Map[String,String],
                   morfcode: String, isPart: Boolean, n: String, gysMode: Boolean,
                   gysParticles: Map[String, String]):String =
  {
    val s0 = morfcode.replaceAll("\\{.*", "").replaceAll("ongeanalyseerd", "999").replaceAll("[A-Za-z]","")

    val s1 =  "0" * Math.max(0, 3 - s0.length) + s0

    val pos = tagMapping.getOrElse(s1, tagMapping("999")) // s"MISSING_MAPPING($s/($s1))")

    val posAdapted = // pas op deze code werkt alleen voor CRM!!!
      if (!isPart) pos else if (!gysMode) {
        if (pos.contains("WW")) {
          if (morfcode.equals("285")) "ADV(bw-deel-ww)" else pos.replaceAll("\\)", ",hoofddeel-ww)")
        } else if (pos.contains("BW"))
        { if (morfcode.equals("655")) "BW(adv-pron,vz-deel-bw)" else pos.replaceAll("\\)", ",hoofddeel-bw)") }
        else if (pos.contains("VZ"))
          pos.replaceAll("\\)", ",vz-deel-bw)")
        else if (pos.contains("ADJ")) pos.replaceAll("\\)", ",bw-deel-ww)") // PAS OP, CHECK DEZE
        else pos
      } else { // FOUT kijken WELK stukje de part heeft (of mogelijk hebben)
        val deelSoort = gysParticles.get(n)
        if (deelSoort.isEmpty || !morfcode.contains("{"))
        {
          // Console.err.println(s"Geen deelinfo gevonden voor $n!!!!")
          pos
        } else {
          val soort = deelSoort.get
          if (pos.contains("WW")) {
            if (soort == "bw-deel-ww") "ADV(bw-deel-ww)" else pos.replaceAll("\\)", ",hoofddeel-ww)")
          } else if (pos.contains("BW")) {
            if (soort == "vz-deel-bw") "BW(adv-pron,vz-deel-bw)" else pos.replaceAll("\\)", ",hoofddeel-bw)")
          } else pos.replaceAll("\\)", "," + "deel" + ")").replaceAll("\\(,", "(")
        }
      }

    posAdapted
  }
 */
