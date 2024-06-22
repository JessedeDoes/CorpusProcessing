package corpusprocessing.UDCorpora

import corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.conll_u.CONLL


import java.io.{File, PrintWriter}
import scala.xml.Elem

object CONLL2TEI {

  val corpusDir = "/mnt/Projecten/Corpora/UDCorpora/All/ud-treebanks-v2.14/"

  val xmlDir = "/media/jesse/Data/Corpora/UD/ShortSentences/"
  lazy val pretty = new scala.xml.PrettyPrinter(300, 4)
  val maxFiles = Int.MaxValue
  val maxSentLength = 12 // Integer.MAX_VALUE

  var tokens = 0

  def findAllConllIn(path: String): Set[File] = {
    val f = new File(path)
    if (f.isFile && f.getName.endsWith(".conllu")) {
      Set(f)
    } else if (f.isFile()) {
      Set()
    } else {
      val sub = f.listFiles().map(_.getCanonicalPath).toSet

      sub.flatMap(x => {
        findAllConllIn(x)
      })
    }
  }

  def toTEI(f: java.io.File, setName: String = "Unknown", maxSentLength: Int = Integer.MAX_VALUE): Elem = {
    val sentences = CONLL.parseFile(f)
      .filter(_.tokens.size <= maxSentLength)
      .zipWithIndex.map({ case (s, i) => s.copy(sent_id = s"sentence_$i") })
      .map(_.asTEI())

    val nTokens = sentences.map(x => (x \\ "w").size).sum
    val langCode = corpusprocessing.UDCorpora.languageCodes.two2three(f.getName.replaceAll("_.*", ""))
    val classification: Seq[LangInfo] = Families.getClassificationMapped(langCode)


    val langName = setName.replaceAll("-.*", "").replaceAll("_", " ")
    println(s"$langName $langCode $classification")
    val id = f.getName
    <TEI xml:id={id} xmlns="http://www.tei-c.org/ns/1.0">
      <teiHeader>
        <fileDesc>
          <titleStmt>
            <title type="main">
              {f.getName}
            </title>
            <title type="sub">
              {setName}
            </title>
            <respStmt>
              <resp>Conversion to TEI</resp>
              <name>Instituut voor de Nederlandse Taal</name>
            </respStmt>
          </titleStmt>
          <extent>
            <!--These numbers do not reflect the size of the sample!-->
            <measure quantity={sentences.size.toString} unit="sentences" xml:lang={langCode}/>
            <measure quantity={nTokens.toString} unit="words" xml:lang="en"/>
          </extent>

          <publicationStmt>
            <publisher>Instituut voor de Nederlandse Taal</publisher>
            <pubPlace>Leiden</pubPlace>
            <date>2023</date>
            <availability>
              <p>Via online corpus application: https://corpora.ato2.ivdnt.org</p>
            </availability>
          </publicationStmt>
        </fileDesc>
        <sourceDesc>
          <listBibl type="intMetadata">
            <bibl type="intMetadata">
               <interpGrp type="languageClassification"> {
                 classification.map(c => <interp type={c.level}>{c.name}</interp>)
                 }
               </interpGrp>
              {
                classification.take(5).zipWithIndex.map({case (c, i) => <interpGrp type={s"languageClassification.$i"}><interp>{c.name}</interp></interpGrp>})
              }
            </bibl>
          </listBibl>
        </sourceDesc>
        <encodingDesc>
          <langUsage>
            <language ident={langCode} usage={nTokens.toString}>
              {langName}
            </language>
          </langUsage>
        </encodingDesc>
      </teiHeader>

      <text>

        <body>
          <div>
            <p>
              {sentences}
            </p>
          </div>
        </body>
      </text>
    </TEI>
  }
  def main(args: Array[String]) = {
    val in = args.headOption.getOrElse(corpusDir)
    val f = new File(in)
    if (f.isFile) {
      val tei = toTEI(f)
      val pw = new PrintWriter(s"/tmp/${f.getName.replaceAll("[.][^.]*$", "")}.xml")
      pw.println(tei)
      pw.close()
    } else {
      val files = findAllConllIn(in)
      files.take(maxFiles).foreach(f => {

        val setName = f.getParentFile.getName.replaceAll("UD_", "")
        val tei = toTEI(f, setName, maxSentLength = maxSentLength)
        val nTokens = (tei \\ "w").size
        tokens = tokens + nTokens

        val underscores = (tei \\ "w").filter(_.text == "_").size

        val hasContent = 4 * underscores < nTokens
        if (hasContent) {
          val pw = new PrintWriter(s"$xmlDir/${f.getName.replaceAll("[.][^.]*$", "")}.xml")
          pw.println(pretty.format(tei))
          pw.close()
        } else {
          Console.err.println(s"Corpus $f has too many void tokens: $underscores of $nTokens")
        }

        println(s"Converted:  ${f.getName}, $nTokens tokens, total $tokens now")
      })
    }

    //p.foreach(println)
  }
}
