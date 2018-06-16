package eindhoven

object pronominabel {
  val gradables = """veel
    weinig
    beide
    meer
    teveel
    minder
    keiveel
    meeste
    evenveel
    zoveel
    mindere
    vele
    superveel
    keiweinig
    allerminst
    minst
    veels
    zovele
    vaak""".split("\\s+").toSet // waarom 'beide' grad??



  implicit def makeList(s: String):List[String] = s.split(",").toList

  val wordformFeatureMap:Map[String,List[String]] = Map(
    "ik" -> "vol",
    "ikke" -> "nadr",
    "'k" -> "red",

    "wij" -> "vol",
    "we" -> "red",

    "je" -> "red",
    "jij" -> "vol",
    "jou" -> "vol,obl",

    "ge" -> "red",
    "gij" -> "vol",


    "de" -> "rest",

    "hij"-> "vol",
    "ie" -> "red",
    "hem" -> "obl",
    "hemzelf" -> "nadr,obl",
    "'m" -> "red,obl",

    "ze" -> "red",
    "zij" -> "vol",

    "haar" -> "obl,vol,3",
    "haarzelf" -> "obl,nadr,3v,fem", // VNW(pers,pron,obl,nadr,3v,getal,fem)

    "hen" -> "obl,vol,3p",
    "hun" -> "obl,vol,3p",
    "henzelf" -> "obl,nadr,3p",
    "hun" -> "obl,nadr,3p"
  )

  val lemmaFeatureMap:Map[String,List[String]] = Map(
    "ik" -> "1,ev",
    "ikzelf" -> "nadr,1,ev",
    "wij" -> "1,mv",
    "wijzelf" -> "nadr,1,mv",

    "jij" -> "2v,ev",
    "jijzelf" -> "nadr,2v,ev",



    "u" -> "vol,2b",

    "gij" -> "2,getal",
    "gijzelf" -> "nadr,2",

    "hij" -> "3,ev,masc",
    "hijzelf" -> "nadr,3m,ev,masc",
    "men" -> "red,3p,ev,masc",

    "zij_ev"  -> "3,fem",
    "zij_mv" -> "3,x-mv",

    "uzelf" -> "nadr,2b,getal",
    "jullie" -> "2,mv",
    "ikzelf" -> "1,ev,nadruk",
    // "het" -> "evon", // alleen bij LID
  )

  import Eindhoven._

  def enhancePronFeatures(word: String, lemma: String, tag: String) =
    {
      val lem1 = if (Set("ze","zij").contains(lemma.toLowerCase)) {
        if (tag.matches(".*pers.*nomin.*") && tag.contains("ev"))
          "zij_ev" else if (tag.contains("mv") || tag.contains("obl"))
          "zij_mv" else "zij"
      } else lemma

      val lemExtra:List[String] = lemmaFeatureMap.getOrElse(lem1.toLowerCase(),List())
      val wordExtra:List[String] = wordformFeatureMap.getOrElse(word.toLowerCase(),List())


      addFeatures(tag, lemExtra ++ wordExtra)
    }
}


/*
[T501a]	VNW(pers,pron,nomin,vol,1,ev)	ik
[T501b]	VNW(pers,pron,nomin,nadr,1,ev)	ikzelf, ikke
[T501c]	VNW(pers,pron,nomin,red,1,ev)	’k
[T501d]	VNW(pers,pron,nomin,vol,1,mv)	wij
[T501e]	VNW(pers,pron,nomin,nadr,1,mv)	wijzelf
[T501f]	VNW(pers,pron,nomin,red,1,mv)	we
[T501g]	VNW(pers,pron,nomin,vol,2v,ev)	jij
[T501h]	VNW(pers,pron,nomin,nadr,2v,ev)	jijzelf
[T501i]	VNW(pers,pron,nomin,red,2v,ev)	je
[U501j]	VNW(pers,pron,nomin,vol,2b,getal)	u
[U501k]	VNW(pers,pron,nomin,nadr,2b,getal)	uzelf
[U501l]	VNW(pers,pron,nomin,vol,2,getal)	gij
[U501m]	VNW(pers,pron,nomin,nadr,2,getal)	gijzelf
[U501n]	VNW(pers,pron,nomin,red,2,getal)	ge

[U501o]	VNW(pers,pron,nomin,vol,3,ev,masc)	hij
[T501p]	VNW(pers,pron,nomin,nadr,3m,ev,masc)	hijzelf
[U501q]	VNW(pers,pron,nomin,red,3,ev,masc)	ie
[U501r]	VNW(pers,pron,nomin,red,3p,ev,masc)	men
[T501s]	VNW(pers,pron,nomin,vol,3v,ev,fem)	zij
[T501t]	VNW(pers,pron,nomin,nadr,3v,ev,fem)	zijzelf
[U501u]	VNW(pers,pron,nomin,vol,3p,mv)	zij
[U501v]	VNW(pers,pron,nomin,nadr,3p,mv)	zijzelf
[T502a]	VNW(pers,pron,obl,vol,2v,ev)	jou
[U502b]	VNW(pers,pron,obl,vol,3,ev,masc)	hem
[T502c]	VNW(pers,pron,obl,nadr,3m,ev,masc)	hemzelf
[U502d]	VNW(pers,pron,obl,red,3,ev,masc)	’m
[U502e]	VNW(pers,pron,obl,vol,3,getal,fem)	haar
[U502f]	VNW(pers,pron,obl,nadr,3v,getal,fem)	haarzelf
[U502g]	VNW(pers,pron,obl,red,3v,getal,fem)	’r, d’r
[U502h]	VNW(pers,pron,obl,vol,3p,mv)	hen, hun
[U502i]	VNW(pers,pron,obl,nadr,3p,mv)	henzelf, hunzelf
[U503a]	VNW(pers,pron,stan,nadr,2v,mv)	jullie
[U503b]	VNW(pers,pron,stan,red,3,ev,onz)	het, ’t
[U503c]	VNW(pers,pron,stan,red,3,ev,fem)	ze
[U503d]	VNW(pers,pron,stan,red,3,mv)	ze
[T504a]	VNW(pers,pron,gen,vol,1,ev)	mijns gelijke, gedenk mijner
[T504b]	VNW(pers,pron,gen,vol,1,mv)	ons gelijke, velen  onzer
[U504c]	VNW(pers,pron,gen,vol,2,getal)	uws gelijke, wie  uwer
[T504d]	VNW(pers,pron,gen,vol,3m,ev)	zijns gelijke,  zijner
[U504e]	VNW(pers,pron,gen,vol,3v,getal)	haars gelijke, harer
[U504f]	VNW(pers,pron,gen,vol,3p,mv)	huns gelijke, een  hunner
 */