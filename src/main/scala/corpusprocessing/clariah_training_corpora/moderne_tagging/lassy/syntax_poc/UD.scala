package corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.syntax_poc

import corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.syntax_poc.RelQueryTests

import java.io.PrintWriter
import scala.collection.immutable
import scala.xml._

object UD
{
  val relations: List[(String, String)] = List(
  "acl" -> "clausal modifier of noun (adnominal clause)",
  "acl:relcl" -> "relative clause modifier",
  "advcl" -> "adverbial clause modifier",
  "advmod" -> "adverbial modifier",
  "advmod:emph" -> "emphasizing word, intensifier",
  "advmod:lmod" -> "locative adverbial modifier",
  "amod" -> "adjectival modifier",
  "appos" -> "appositional modifier",
  "aux" -> "auxiliary",
  "aux:pass" -> "passive auxiliary",
  "case" -> "case marking",
  "cc" -> "coordinating conjunction",
  "cc:preconj" -> "preconjunct",
  "ccomp" -> "clausal complement",
  "clf" -> "classifier",
  "compound" -> "compound",
  "compound:lvc" -> "light verb construction",
  "compound:prt" -> "phrasal verb particle",
  "compound:redup" -> "reduplicated compounds",
  "compound:svc" -> "serial verb compounds",
  "conj" -> "conjunct",
  "cop" -> "copula",
  "csubj" -> "clausal subject",
  "csubj:outer" -> "outer clause clausal subject",
  "csubj:pass" -> "clausal passive subject",
  "dep" -> "unspecified dependency",
  "det" -> "determiner",
  "det:numgov" -> "pronominal quantifier governing the case of the noun",
  "det:nummod" -> "pronominal quantifier agreeing in case with the noun",
  "det:poss" -> "possessive determiner",
  "discourse" -> "discourse element",
  "dislocated" -> "dislocated elements",
  "expl" -> "expletive",
  "expl:impers" -> "impersonal expletive",
  "expl:pass" -> "reflexive pronoun used in reflexive passive",
  "expl:pv" -> "reflexive clitic with an inherently reflexive verb",
  "fixed" -> "fixed multiword expression",
  "flat" -> "flat multiword expression",
  "flat:foreign" -> "foreign words",
  "flat:name" -> "names",
  "goeswith" -> "goes with",
  "iobj" -> "indirect object",
  "list" -> "list",
  "mark" -> "marker",
  "nmod" -> "nominal modifier",
  "nmod:poss" -> "possessive nominal modifier",
  "nmod:tmod" -> "temporal modifier",
  "nsubj" -> "nominal subject",
  "nsubj:outer" -> "outer clause nominal subject",
  "nsubj:pass" -> "passive nominal subject",
  "nummod" -> "numeric modifier",
  "nummod:gov" -> "numeric modifier governing the case of the noun",
  "obj" -> "object",
  "obl" -> "oblique nominal",
  "obl:agent" -> "agent modifier",
  "obl:arg" -> "oblique argument",
  "obl:lmod" -> "locative modifier",
  "obl:tmod" -> "temporal modifier",
  "orphan" -> "orphan",
  "parataxis" -> "parataxis",
  "punct" -> "punctuation",
  "reparandum" -> "overridden disfluency",
  "root" -> "root",
  "vocative" -> "vocative",
  "xcomp" -> "open clausal complement")

  val dutchHTML = XML.load("doc/ud_rel.html")

  val relsFromHTML = (dutchHTML \\ "li").flatMap(l => {
    val txt = l.text
    val rels = (l \ "strong").text.trim.split("\\s*,\\s*")
    rels.map(r => <tr><td>[] --{r}--> []</td><td>{l.child}</td></tr>)
   }
  )

  val p = new scala.xml.PrettyPrinter(80, 4)
  val table = <table>{relsFromHTML}</table>

  def tryAll() = {
    relations.foreach({ case (rel, gloss) => {
      val z = rel
      val q = s"rel('dep::$rel')"
      val n = s"$rel ($gloss)"
      RelQueryTests.testRelQueryDirect(n, q)
    }
    })
  }
  def main(args: Array[String])  = {
    val pw = new PrintWriter("/tmp/table.html")
    pw.println(p.format(table))
    pw.close()
  }
}
  
