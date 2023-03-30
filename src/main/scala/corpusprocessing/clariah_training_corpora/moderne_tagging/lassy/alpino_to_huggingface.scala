package corpusprocessing.clariah_training_corpora.moderne_tagging.lassy

import grouping.groupWithFirst
import org.json4s._
import org.json4s.jackson.Serialization.write

import java.io.PrintWriter
import scala.xml._






object alpino_to_huggingface {

  implicit val formats = DefaultFormats

  def parseFile(f: java.io.File): Seq[UdSentence] = {
    val lines = io.Source.fromFile(f).getLines.toStream
    val language = f.getName.replaceAll("_.*","")
    val grouped = groupWithFirst[String](lines, x=> x.startsWith("# sent_id =") || x.startsWith("# S-ID")) // S-ID in Japanese KTC

    val sentences = grouped.flatMap(g => {
      val sent_id = g.find(x => x.startsWith("# sent_id") || x.startsWith("# S-ID")).map(_.replaceAll(".*[=:]","").replaceAll("\\s+",""))

      sent_id.map(x => {
        val text =  g.find(_.startsWith("# text")).map(_.replaceAll(".*=","").trim)
        val tokens = g.filter(_.matches("[0-9].*")).map(_.split("\\t").toList).map(l => UdToken(l(0),l(1),l(2),l(3),l(4),l(5),l(6),l(7),l(8),l(9), x, language))
        val sentence = UdSentence(x, language, tokens)
        sentence })
    })
    sentences
  }

  def makeTEI(f: java.io.File, sentences: Seq[UdSentence]) = {
    val documents: Seq[(String, Array[(UdSentence, Int)])] = grouping.groupBy[UdSentence,String](sentences, x => x.filename)
    <TEI xmlns="http://www.tei-c.org/ns/1.0" xml:id={f.getName}>
      <text>
        <body>
          <div>
            <ab>
              {documents.map{case (did, sents) => {
                val sentsindoc = sents.map(_._1)
                val paragraphs: Seq[(String, Array[(UdSentence, Int)])] = grouping.groupBy[UdSentence,String](sentsindoc, x => x.paragraph)
                <docje xml:id ={did}>
                  {paragraphs.map{case (pid, sents) =>
                     val sentsinpar: Array[UdSentence] =  sents.map(_._1)
                     <p xml:id ={pid}>
                     {sentsinpar.map(_.TEI).toSeq}
                     </p>
                 }}
                </docje>
            }}}
            </ab>
          </div>
        </body>
      </text>
    </TEI>
  }


  //lazy val allFiles: Seq[java.io.File] = new java.io.File(".").listFiles.filter(_.isDirectory()).toList.flatMap(_.listFiles.toList).filter(_.getName.matches("[a-z][a-z]_pud-ud-test.conllu"))

  val lassy_training_file = "/mnt/Projecten/Corpora/TrainingDataForTools/LassyKlein/TrainTest/treebank.train_dev.conll"

  def main(args: Array[String]): Unit = {

    val f = new java.io.File(if (args.size > 0) args(0) else lassy_training_file)
    val udsents :  Seq[UdSentence] = parseFile(f)

    val sents: Seq[Sentence] = udsents.map(_.sent)
    val s1: Seq[Sentence] = sents.zipWithIndex.map({ case (s, i) => s.copy(id = i.toString) })

    // write json
    val jsons = s1.map(s => write(s))
    val pw = new PrintWriter("/tmp/huggie.json")
    jsons.take(Integer.MAX_VALUE).foreach(pw.println)
    pw.close()


    // write XML

    val p = new PrettyPrinter(80,2)
    val xml = p.format(makeTEI(f,udsents))

    val pw1 = new PrintWriter("/tmp/lassy.xml")
    pw1.print(xml)
    pw1.close()
  }
}


/*
 # newdoc id = n01001
 # sent_id = n01001011
 # text = 「米国でデジタルへの移行が大いに進んでいる一方で、権力の平和的な移行は進んでいない」と書いたブログを、オバマ大統領特別補佐コリ・シューマンが月曜日に投稿した。
 # text_en = “While much of the digital transition is unprecedented in the United States, the peaceful transition of power is not,” Obama special assistant Kori Schulman wrote in a blog post Monday.
 1       「      「      PUNCT   補助記号-括弧開 _       2       punct   _       BunsetuPositionType=CONT|LUWBILabel=B|LUWPOS=補助記号-括弧開|SpaceAfter=No|UniDicLemma=「
 2       米国    米国    PROPN   名詞-固有名詞-地名-国   _       10      obl     _       BunsetuPositionType=SEM_HEAD|LUWBILabel=B|LUWPOS=名詞-固有名詞-地名-国|SpaceAfter=No|UniDicLemma=米国
 3       で      で      ADP     助詞-格助詞     _       2       case    _       BunsetuPositionType=SYN_HEAD|LUWBILabel=B|LUWPOS=助詞-格助詞|SpaceAfter=No|UniDicLemma=で
 4       デジタル        デジタル        NOUN    名詞-普通名詞-形状詞可能        _       7       nmod    _       BunsetuPositionType=SEM_HEAD|LUWBILabel=B|LUWPOS=名詞-普通名詞-一般|SpaceAfter=No|UniDicLemma=デジタル



     ID: Word index, integer starting at 1 for each new sentence; may be a range for multiword tokens; may be a decimal number for empty nodes (decimal numbers can be lower than 1 but must be greater than 0).
     FORM: Word form or punctuation symbol.
     LEMMA: Lemma or stem of word form.
     UPOS: Universal part-of-speech tag.
     XPOS: Language-specific part-of-speech tag; underscore if not available.
     FEATS: List of morphological features from the universal feature inventory or from a defined language-specific extension; underscore if not available.
     HEAD: Head of the current word, which is either a value of ID or zero (0).
     DEPREL: Universal dependency relation to the HEAD (root iff HEAD = 0) or a defined language-specific subtype of one.
     DEPS: Enhanced dependency graph in the form of a list of head-deprel pairs.
     MISC: Any other annotation.
 */


/*
                                Table "public.tag_mapping_cgn"
    Column    |  Type   | Collation | Nullable |                    Default
--------------+---------+-----------+----------+-----------------------------------------------
 id           | text    |           |          |
 cgn          | text    |           |          |
 tdn          | text    |           |          |
 tdn_core     | text    |           |          |
 example      | text    |           |          |
 verdacht     | boolean |           |          | false
 pkid         | integer |           | not null | nextval('tag_mapping_cgn_pkid_seq'::regclass)
 tdn_core_org | text    |           |          |
 comment      | text    |           |          |
Indexes:
    "tag_mapping_cgn_pkey" PRIMARY KEY, btree (pkid)


 */