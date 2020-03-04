package posmapping

import scala.util.{Failure, Success, Try}

object HistoricalTagsetPatching {

  val bw2aa = false

  val advSubtypeMap = Map("demonstrative" -> "dem", "general" -> "gen", "indefinite" -> "indef", "interrogative" -> "inter", "negative" -> "neg", "relative" -> "rel")


  def getFeature(tag: String, name: String) = {
    val r = s"$name=([^,()]*)".r
    r.findFirstMatchIn(tag).map(_.group(1))
  }

  def patchPoSMistakes(m: String, p:String, w: String): String =
  {
    // println(s"$m\t$p")
    val infl = getFeature(p,"inflection").getOrElse("other")
    val p1 = if (p.startsWith("NUM") && p.contains("indef")) {
      p.replaceAll("NUM", "PD")
    }

    else if (p.startsWith("NOU")) {
      val posNew = if (p.contains("type=proper")) "NOU-P" else "NOU-C"
      if (p.matches(".*type=(common|proper).*")) p.replaceAll("type=[a-z]+(,?)","").replaceAll("^[A-Z]+", posNew) else p
    }

    else if (p.startsWith("ADV")) {
      val typ = if (p.contains("pron")) "pron" else "general"
      val st0 = getFeature(p,"type").getOrElse("other")
      val subtype = advSubtypeMap.getOrElse(st0, st0)

      if (st0 == "general" && bw2aa) // dit is een nog twijfelachtige beslissing --- wanneer kan je dit doen?????
        s"""AA(position=adverbial,inflection=$infl)""" ;// nee inflectie is weg zo...
      else  {
        if (!subtype.startsWith("gen"))
          s"""ADV(type=$typ,subtype=$subtype,inflection=$infl)"""
        else  s"""ADV(type=$typ,inflection=$infl)"""
      }
    }

    else if (p.startsWith("ADP")) {
      p.replaceAll("type=general(,?)","")
    }

    else if (p.startsWith("ADJ")) {
      p.replaceAll("ADJ","AA").replaceAll("\\)",",position=pred|prenom|postnom)").replaceAll("number=","NA=") // Vervelend!!!
    }


    else if (p.startsWith("VRB") && p.contains("=part")) {
      val tense = if (w.toLowerCase().endsWith("nde")) "pres" else "past"
      p.replaceAll("\\)", s",tense=" + tense + ")").replaceAll("number=","NA=")
    }

    else if (p.startsWith("CON")) {
      val typ = getFeature(p,"type").getOrElse("other")
      val ntyp = if (p.contains("coord")) "coord" else if (p.contains("subo")) "sub" else "general"
      val subtype = if (ntyp == "general") Some(typ) else None
      if (subtype.isDefined) s"""CONJ(type=other,subtype=${subtype.get},inflection=$infl)""" else s"""CONJ(type=$ntyp,inflection=$infl)"""
    }

    else if (p.startsWith("PD")) {
      val typ = getFeature(p,"type").getOrElse("other")
      if (typ == "art")
      {
        p.replaceAll("type=art", "type=dem").replaceAll("subtype", "subtype_art")
      } else p
    }
    else if (m.startsWith("25") && p.contains("=finite")) p.replaceAll("=finite", "=inf")  else p

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

    val integratedTag = Try(IntegratedTagset.IntegratedTag(p2)) match {
      case Success(t) =>
      case Failure(exception) => Console.err.println(s"exception for $p2: $exception")
    }

    println(s"$m\t$p\t$p2")
    p2
  }
}
