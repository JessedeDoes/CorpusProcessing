package corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.conll_u.ud_parsing
import java.io.File
import javautil.MultipartFormDataClient
import scala.collection.JavaConverters._
import collection.convert

// https://docs.scala-lang.org/toolkit/http-client-upload-file.html
object udParserClient {

  def postFile(f: File)  = {
    val c = new MultipartFormDataClient("http://svprre10.ivdnt.loc:7860/api/v1/input", "utf-8")
    c.addFilePart("file", f)
    val r = c.finish().asScala.toList
    r.foreach(println)
  }

  def main(args: Array[String]): Unit = {
    val test = new java.io.File("/home/jesse/Downloads/zinnetje.txt")
    postFile(test)
  }
}
