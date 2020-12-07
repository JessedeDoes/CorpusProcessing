package corpusprocessing.mimore
import scala.xml._
import utils.PostProcessXML._

object MandSplit {
  val inDir = new java.io.File("/mnt/Projecten/Corpora/Historische_Corpora/Mimore/Mand_interviews/xml")
  val outdir = new java.io.File("/mnt/Projecten/Corpora/Historische_Corpora/Mimore/Mand_interviews/xml_split")

  def fixItem(i: Elem) = {
    val tags = (i \ "tags").text.split("\\s+").toList
    val words = (i \ "s").filter(x => (x \ "@type").text == "item").text.split("\\s+").toList
    if (tags.size >=1 && tags.size == words.size)
      {
        val split = <s type="split-answer">{words.zip(tags).map{case (w,t) => <word><token>{w}</token><tag type="sand">{t}</tag></word>}}</s>
        println(split)
        i.copy(child = i.child ++ Seq(split))
      } else if (tags.size == 1)
      {
        val split = <s type="split-answer"><word><token>{words.mkString("_")}</token><tag type="sand">{tags(0)}</tag></word></s>
        i.copy(child = i.child ++ Seq(split))
      } else i
  }

  def doit(in: String, out: String) = {
    val inDoc = XML.load(in)
    val splitDoc = updateElement(inDoc, _.label=="item", fixItem)
    XML.save(out, splitDoc, "UTF-8")
  }

  def main(args: Array[String]): Unit = {
    utils.ProcessFolder.processFolder(inDir, outdir, doit)
  }
}

/*
    <item>
                <s type="item">adem</s>
                <s type="transcription">a.i2_d6m</s>
                <s type="transcription_vereenvoudigd">aaidem</s>
                <s type="transcription_ipa">aˑɪ̲dəm</s>
                <tags>N(onbek,3,s)</tags>
    </item>

        <s type="split-answer">
                <word>
                <token>waffer</token>
                        <lemma>welke</lemma>
                        <tag type="edisyn">Pron(wh,pl,prenom)</tag>
                        <tag type="cgn">N(soort,ev,basis,onz,stan)</tag>
        </word>
                <word>
                <token>boeken</token>
                        <lemma>boek</lemma>
                        <tag type="edisyn">N(pl)</tag>
                        <tag type="cgn">N(soort,mv,basis)</tag>
        </word>
                </s>
 */