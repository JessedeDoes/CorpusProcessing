package corpusprocessing.clariah_training_corpora.training_data_extraction.specific
import corpusprocessing.clariah_training_corpora.training_data_extraction._
object quotation_corpora {

}


case class QuotationCorpus(sourceFolder: String, name: String) extends extract_training_data_trait {
  override val split_test_train_on_document_level: Boolean = true
  override lazy val output_prefix: String = name
  override val max_files: Int = Integer.MAX_VALUE
  override val training_subsets: Int = 10
  override lazy val output_folder: String = sourceFolder + "/" + "test_train" + (if (training_subsets > 1) "/partitioned/" else "")
  // new java.io.File(output_folder).mkdir()
  override lazy val default_input_folder = sourceFolder + "/" + "CobaltServeExport"
}

object QuotationCorpora {
  val baseDir  = "/mnt/Projecten/Corpora/TrainingDataForTools/galahad-corpus-data/public-corpora/clariah-evaluation-corpora/"
}

import corpusprocessing.clariah_training_corpora.patch_scripts.fixTokenization.getId
import corpusprocessing.clariah_training_corpora.training_data_extraction.specific.QuotationCorpora._
import utils.PostProcessXML

object q15 extends QuotationCorpus(baseDir + "gtbcit_15", "gtbcit_15")
object q16 extends QuotationCorpus(baseDir + "gtbcit_16", "gtbcit_16")
object q17 extends QuotationCorpus(baseDir + "gtbcit_17", "gtbcit_17")
object q18 extends QuotationCorpus(baseDir + "gtbcit_18", "gtbcit_18")
object q19 extends QuotationCorpus(baseDir + "gtbcit_19", "gtbcit_19")

object qMix extends QuotationCorpus("aap", name="gtbcit") {
  val corpora = List(q15,q16,q17,q18,q19)
  override lazy val output_folder: String =  "/tmp/"
  val folders = corpora.map(_.default_input_folder).toArray
  override def main(args: Array[String])  = {
    super.main(folders)
  }
}

import scala.xml._


object headertje {
  def header(dict: String, id: String, lemma: String): Elem =
    <teiHeader>
      <fileDesc>
        <titleStmt>
          <title>
            Quotation
            {id}
            from dictionary
            {dict.toUpperCase()}{if (lemma.nonEmpty) s", lemma $lemma" else ""}
          </title>
        </titleStmt>
        <publicationStmt>
          <p>
            Publish by INT (http://www.ivdnt.org) as training/evaluation data for lemmatization and part of speech tagging in
          </p>
        </publicationStmt>
        <sourceDesc>
          <p>
            INT historical dictionaries, cf https://gtb.ivdnt.org
          </p>
        </sourceDesc>
      </fileDesc>
    </teiHeader>


  def groupHeader(dict: String, century: String, partition: String): Elem =
    <teiHeader>
      <fileDesc>
        <titleStmt>
          <title>
            Quotations from dictionary
            {dict.toUpperCase()}
            ,
            {century}
            th century, partition
            {partition}
          </title>
        </titleStmt>
        <publicationStmt>
          <p>
            Publish by INT (http://www.ivdnt.org) as training/evaluation data for lemmatization and part of speech tagging in
          </p>
        </publicationStmt>
        <sourceDesc>
          <p>
            INT historical dictionaries, cf https://gtb.ivdnt.org
          </p>
        </sourceDesc>
      </fileDesc>
    </teiHeader>
}

import scala.util.{Failure, Success, Try}
object boilerplate {

  val bad = Set("entry-id", "partition", "sense-id", "timeSpan", "modern-lemma", "lexicon", "time", "valid")
  val wOnly = Set("lemma", "pos")
  def removeBadAttributes(e: Elem)  = {

    if (e.label == "q") e.copy(attributes = e.attributes.filter(x => false))
    else {
      e.copy(attributes = e.attributes.filter(x => !bad.contains(x.key) && !(!Set("w", "pc").contains(e.label) && wOnly.contains(x.key))))
    }
  }


 def removeBadAttributesIn(d: Elem)  = {
   val d1 = PostProcessXML.updateElement3(d, x => true, removeBadAttributes)
   val d2 = PostProcessXML.updateElement(d1, _.label=="date", d => <bibl>{d}</bibl>)
   d2
 }
  def wrapFile(f: String) = {
    Try {
      val doc = XML.load(f)
      val id = getId(doc)
      val eid = (doc \ "@entry-id").text
      val dict = if (eid.startsWith("mnw")) "MNW" else "WNT"
      val lemma = (doc \ "@lemma").text
      <TEI xmlns="http://www.tei-c.org/ns/1.0">
        {headertje.header(dict, id, lemma)}<text>
        <body>
          <div>
            {doc}
          </div>
        </body>
      </text>
      </TEI>
    } match {
      case Success(x) => removeBadAttributesIn(x)
      case Failure(x) => println(s"Failed on $f"); <bummer/>
    }
  }

  def fix(in: String, out: String)  = {
    if (in.endsWith(".xml"))
      XML.save(out, wrapFile(in))
  }
  val docje = "/mnt/Projecten/Corpora/TrainingDataForTools/GTBCIT_TEMP/gtbcit_15/dev/docpid_954.xml"
  import java.io.File
  val in = new File("/mnt/Projecten/Corpora/TrainingDataForTools/GTBCIT_TEMP/")
  val out = new File("/tmp/fixcit/")


  def foldersIn(f: File): List[File] = if (f.isFile) List() else if (Set("test", "train", "dev").contains(f.getName))
    List(f) else f.listFiles.toList.flatMap(foldersIn)


  def combineFilesIn(f: File) = {
    println(s"${f.listFiles.size} in ${f.getCanonicalPath}")
    val partition = f.getName
    val century = f.getParentFile.getName.replaceAll(".*_([0-9]{2}).*", "$1")
    val docs = f.listFiles.filter(f => f.getName.endsWith(".xml") && !f.getName.contains("quotations")).toList.map(XML.loadFile)
    val doc = docs.head

    val eid = (doc \ "@entry-id").text
    val dict = if (eid.startsWith("mnw")) "MNW" else "WNT"

    <TEI xmlns="http://www.tei-c.org/ns/1.0">
      {headertje.groupHeader(dict, century, partition)}<text>
      <body>
        <div>
          {docs.map(removeBadAttributesIn)}
        </div>
      </body>
    </text>
    </TEI>
  }
  def main(args: Array[String]) = {

    val folders = foldersIn(in)
    folders.foreach(f =>
      {
        println(f.getCanonicalPath)
        val combined = combineFilesIn(f)
        //val outputFileName =   "/tmp/" + f.getName + ".quotations.xml" /* f.getParentFile.getName  */
        val outputFileName = s"${f.getCanonicalPath}/${f.getParentFile.getName}.${f.getName}.quotations.xml"
        println(s"Save $f to $outputFileName")
        XML.save(outputFileName, combined)
      }
    )
    println(folders)
    //println(wrapFile(docje))
    //utils.ProcessFolder.processFolder(in, out, fix)
  }
}
