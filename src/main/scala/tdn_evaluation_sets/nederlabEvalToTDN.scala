package tdn_evaluation_sets
import scala.xml._
import utils.PostProcessXML
object nederlabEvalToTDN {

  val seen: collection.mutable.Set[String] = collection.mutable.Set()
  def doWord(w: Elem): Elem = {
    val tags = (w \ "@ctag").text.replaceAll("\\s*&\\s*","+")
    val word = w.text
    val lemma = (w \ "@lemma").text


    val mappie = tags.replaceAll("[|].*","").split("\\+").map(tag => {

      if (!Settings.mapping.contains(tag)) {
        if (!seen(tag)) println(s"--> <$tag> (part of $tags; word=$word) NOT FOUND")
        seen.add(tag)
      }
      val mappedTags = Settings.mapping.getOrElse(tag, s"NOPEY_$tag:$tags").replaceAll(";", ",").replaceAll("wf=","WF=")

      val mappedTag1 = mappedTags.split("\\|").map(mappedTag => mapSingleTag(word, lemma, mappedTag)).mkString("|")

      mappedTag1
    }).mkString("+")
    val newAtts = w.attributes.append(new UnprefixedAttribute("pos", mappie, Null))

    w.copy(attributes = newAtts)
  }

  private def mapSingleTag(word: String, lemma: String, mappedTag: String) = {
    val mappedTag1 = if (mappedTag.matches("AA.*")) {
      val mood = if (word.matches(".*er(e?)n?$") && !lemma.endsWith("r")) "comp" else if (word.matches(".*st(e?)n?$") && !lemma.endsWith("st")) "sup" else "pos"
      //println(s"$mood for |$word|")
      mappedTag.replaceAll("degree=uncl", "degree=" + mood)
    } else if (mappedTag.contains("=art")) {
      if (lemma == "een")
        mappedTag.replaceAll("type=article", "type=indef")
      else
        mappedTag.replaceAll("type=article", "type=d-p")
    } else if (mappedTag.startsWith("PD")) {
      val m0 = mappedTag
        .replaceAll("=dem", "=d-p")
        .replaceAll("=rel", "=d-p")
        .replaceAll("inter.rel", "w-p")
      if (m0.equals("PD(type=indef,position=prenom)"))
        if (lemma == "een")
        "PD(type=indef,subtype=art,position=prenom)"
      else "PD(type=indef,subtype=oth,position=prenom)"
      else m0
    } else if (mappedTag.startsWith("NOU-C")) {
      if (word.endsWith("en") && !lemma.endsWith("en"))
        "NOU-C(number=pl)"
      else mappedTag
    }  else if (mappedTag.startsWith("VRB"))
    {
      if (mappedTag.contains("finiteness=fin"))
        if (word.matches(".*(de|te)n?$") && !lemma.matches(".*[td]en")) mappedTag.replaceAll("tense=uncl", "tense=past")
        else if (word.matches(".*[td]$") && !lemma.matches(".*[td]en") )
          mappedTag.replaceAll("tense=uncl", "tense=pres")
        else mappedTag
      else mappedTag
    }  else mappedTag.replaceAll("^CON([^J])", "CONJ$1")
    //if (!Settings.coreTags.contains(mappedTag1)) Console.err.println(s"Kannie: $mappedTag1")
    mappedTag1
  }

  def map(d: Elem): Elem = {
    val d1 = PostProcessXML.updateElement(d, _.label=="w", doWord)
    d1
  }
}
