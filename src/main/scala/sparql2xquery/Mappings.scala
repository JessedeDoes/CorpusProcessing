package sparql2xquery

object Mappings
{
  type PredicateMapping = Map[String, BasicPattern]

  val testMap:PredicateMapping = Map(
    "http://example.org/su" ->
      BasicPattern(
        Set("subject←//node[@cat='smain']", "object←$subject/node[@rel='su']")
      ),
    "http://example.org/ob" ->
      BasicPattern(
        Set("subject←//node[@cat='smain']", "object←$subject/node[@rel='obj1']")
      ),
    "http://example.org/word" ->
      BasicPattern(
        Set("object←xs:string($subject/@word)")
      ),
    "http://example.org/lemma" ->
      BasicPattern(
        Set("object←xs:string($subject/@lemma)")
      ),
    "http://example.org/pos" ->
      BasicPattern(
        Set("object←xs:string($subject/@pos)")
      ),
    "http://example.org/rel" ->
      BasicPattern(
        Set("object←xs:string($subject/@rel)")
      ),
    "http://example.org/child" ->
      BasicPattern(
        Set("object←$subject/*")
      ),
    "http://example.org/text" ->
      BasicPattern(
        Set("object←string-join($subject//@word, ' ')")
      ),
    "http://example.org/sentence" ->
      BasicPattern(
        Set("object←$subject/ancestor::alpino_ds/sentence")
      ),
    "http://example.org/precedes" -> // dit zou beter via een filter (where) kunnen worden gedaan
      BasicPattern(
        Set("object←$subject/ancestor::node/descendant::node[xs:int(@begin) > xs:int($subject/@begin)]")
      )
  )

  val udPrefix = "http://universaldependencies.org/u/dep/"

  // this is obviously too complex.
  // zie http://aclweb.org/anthology/W17-0403
  // Increasing return on annotation investment: the automatic construction of
  // a Universal Dependency treebank for Dutch
  // https://github.com/gossebouma/lassy2ud/blob/master/universal_dependencies_2.0.xq

  val udRelMap = Map(

    udPrefix + "nsubj" ->
      BasicPattern(Set(
        "object←$subject/../node[@rel='su']/node[@rel='hd' and @pos='noun']",
        "subject←//node[@rel='hd' and @pos='verb' and parent::node[@cat='smain' or @cat='ssub']]",
        ), Set("subject", "object")),

    udPrefix + "obj" ->
      BasicPattern(Set(
        "object←$subject/../node[@rel='obj1']/node[@rel='hd' and @pos='noun']",
        "subject←//node[@rel='hd' and @pos='verb' and parent::node[@cat='smain' or @cat='ssub']]",
      ), Set("subject", "object")),

    udPrefix + "iobj" ->
      BasicPattern(Set(
        "object←$subject/../node[@rel='obj2']/node[@rel='hd' and @pos='noun']",
        "subject←//node[@rel='hd' and @pos='verb' and parent::node[@cat='smain' or @cat='ssub']]",
      ), Set("subject", "object")),
    udPrefix + "conj" ->
      BasicPattern(
        Set(
          "subject←//node[@rel='cnj']",
          "object←$subject/following-sibling::*[2][@rel='cnj' and preceding-sibling::*[1][@rel='crd']]|$subject/following-sibling::*[1][@rel='cnj']"
        ),
        Set("subject", "object")
      ),
    udPrefix + "cc" ->
      BasicPattern(Set(
        "subject←//node[@rel='cnj']",
        "object←$subject/preceding-sibling::*[1][@rel='crd']"
      ),
    Set("subject", "object")
    ),
    udPrefix + "csubj" ->
      BasicPattern(Set(
        "top←//node",
        "subject←$top/node[@rel='hd' and @pos='verb']",
        "object←$top/node[@rel='su' and (@cat='ssub' or @cat='whsub')]"
      ),
        Set("subject", "object", "top")
      ),
    udPrefix + "xcomp" -> // xcomp heeft een eigen subject (?) anders ccomp ?
      BasicPattern(Set(
        "subject←node[@rel='hd' and @pos='verb' and parent::node[@cat='smain' or @cat='ssub']]",
        "object←$subject/../node[@rel='vc' and ./node[(@cat='ssub' or @cat='whsub') and ./node[@rel='su']]]"
      ),
        Set("subject", "object")
      ),
    udPrefix + "ccomp" -> // ccomp heeft GEEN eigen onderwerp (! deze klopt totaal niet)
      BasicPattern(Set(
        "subject←node[@rel='hd' and @pos='verb' and parent::node[@cat='smain' or @cat='ssub']]",
        "object←$subject/../node[@rel='vc' and ./node[(@cat='ssub' or @cat='whsub') and not(./node[@rel='su'])]]"
      ),
        Set("subject", "object")
      )
  )

  val lassyRelNames = List("top", "su", "det", "hd", "vc", "obj1", "ld", "mod", "predc",
    "mwp", "cnj", "crd", "app", "dp", "cmp", "body", "pc", "posmapping", "nucl", "svp",
    "rhd", "me", "tag", "obj2", "whd", "predm", "dlink", "se", "sup", "hdf",
    "obcomp", "pobj1"
  )

  val dollar = "$"

  val lassyRelMap = lassyRelNames.map(s =>
    s"http://example.org/rel_$s" -> BasicPattern(Set("subject←//node", s"object←${dollar}subject/node[@rel='$s']")))
    .toMap

  val testje = TripleMapping(testMap ++ lassyRelMap ++ udRelMap)
}
