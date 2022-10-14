package corpusprocessing.OGL
import scala.xml._
import utils.{PostProcessXML, ProcessFolder}

import java.io.{PrintWriter, Writer}
object oglGreek {

  def prettyPrint(pw: Writer, e: Elem)  = {
    val pretty = new scala.xml.PrettyPrinter(80, 4)
    pw.write(pretty.format(e))
    pw.close()
  }

  val sample = "/mnt/Projecten/Corpora/Foreign/OGL/Samples/greek.xml"
  def processFile(f: String, o: String)  = {
    println(f)
    val d = XML.load(f)
    val sentences = (d \\ "s").toList
    //println(sentences.map(s => ((s \\ "t") \\ "@p").toList.map(_.text).mkString(";")))
    val grouped = sentences.groupBy(s => ((s \\ "t") \\ "@p").toList.map(_.text).toSet.toList.sorted.mkString(";"))
    val paragraphs = grouped.toList.sortBy({case (p, sents) => {
      (sents.head \ "@n").text.toInt
    }}).map({case (p, sents) => {
        <p n={p}>
          {sents.sortBy(s =>  (s \ "@n").text.toInt)}
        </p>
    }}).toSeq
    val doc = <TEI>

      <teiHeader>
        <sourceDesc>
          <listBibl type="metadata"><bibl>
        {d.attributes.map(a =>
        <interpGrp type={a.key}><interp>{a.value}</interp></interpGrp>
      )}</bibl>
          </listBibl>
        </sourceDesc>
    </teiHeader>
      <text>{paragraphs}</text></TEI>
    val doc1 = PostProcessXML.updateElement(doc, _.label == "t", t => {
      val w = <w pos={(t \ "@o").text.replaceAll("(^.)","$1")}
                 msd={(t \ "@o").text} n={(t \ "@n").text} sourceParagraph={(t \ "@p").text}><seg>{(t \ "f").text}</seg>{t.descendant.filter(x => x.label == "l1" || x.label == "l2").map(l => <lemma>{l.text}</lemma>)}</w>
      w
    })
    prettyPrint(new PrintWriter(o), doc1)
  }
  def main(args:Array[String]): Unit = {
    implicit def f(f: String): java.io.File = new java.io.File(f)
    ProcessFolder.processFolder("/mnt/Projecten/Corpora/Foreign/OGL/Greek/LemmatizedAncientGreekXML-1.2.5/texts", "/mnt/Projecten/Corpora/Foreign/OGL/Greek/PseudoTEI",processFile)
  }
}

/*
<text text-cts="urn:cts:greekLit:tlg0007.tlg021.perseus-grc2" author="Plutarch" title="Pelopidas" file-name="tlg0007.tlg021.perseus-grc2.xml" date-of-conversion="2017-03-29">
  <sentence n="position in the document">
    <token p="passage-level cts urn" n="position in @p" a="nth occurrence in @p" o="Mate tagger POS tag" u="position in s element" join="optional: join b(ack) or a(fter) for punctuation marks" tag="optional: selection of editorial tags containing the token in the original document, such as del or add">
      <wordForm>word form of the token</wordForm>
      <lemma id="contains the id number of the entry in the database consisting of the set of all word forms + morphological analyses of all Greek texts">
        <l1>as many l1 elements as the lemmas found in PerseusUnderPhilologic for
              the relevant word form AND morphological analysis</l1>
        <l2>as many l2 elements as the lemmas found in Morpheus for
              the relevant word form AND morphological analysis</l2>
      </lemma>
    </token>
  </sentence>
  <s n="1">
    <t p="1.1" n="1" a="[1]" o="n-s---mn-" u="1">
      <f>Κάτων</f>
      <l i="63375">
        <l1 o="ne-s---mn-">Κάτων</l1>
      </l>
    </t>
    <t p="1.1" n="2" a="[1]" o="l-s---mn-" u="2">
      <f>ὁ</f>
      <l i="394">
        <l2>ὁ</l2>
      </l>
    </t>
    <t p="1.1" n="3" a="[1]" o="a-s---mn-" u="3">
      <f>πρεσβύτερος</f>
      <l i="53105">
        <l2>πρέσβυς</l2>
      </l>
    </t>
    <t p="1.1" n="4" a="[1]" o="r--------" u="4">
      <f>πρός</f>
      <l>
        <l2>πρός</l2>
      </l>
    </t>
    <t p="1.1" n="5" a="[1]" o="p-p---ma-" u="5">
      <f>τινας</f>
      <l i="18032">
        <l2>τις</l2>
      </l>
    </t>

 */