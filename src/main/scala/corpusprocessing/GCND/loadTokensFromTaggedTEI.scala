package corpusprocessing.GCND

import scala.xml.{Node, XML}

object loadTokensFromTaggedTEI {

  import org.json4s._
  import org.json4s.jackson.Serialization.write

  implicit val formats = DefaultFormats

  val dir = "/mnt/other/svprre10_data/GCND/Tagged/"
  val xml = "@{http://www.w3.org/XML/1998/namespace}"
  lazy val db = GCNDDatabase.db

  def getToken(w: Node) = {
    val pos = (w \ "@pos").text
    val orig = (w \ "@orig").text
    val norm = w.text.trim
    val lemma = (w \ "@lemma").text
    val id = (w \ s"${xml}id").text
    GCNDToken(text_zv = norm, text_lv = orig, pos = pos, lemma = lemma, id = id)
  }


  def toInt(s: String) = {
    try {
      s.toInt
    } catch {
      case e: Exception => println("Boem:" + s); -1
    }
  }

  def getTokenArray(s: Node) = {
    val id = (s \ "w").headOption.map(x => (x \ s"${xml}id").text).getOrElse("-1").replaceAll(".unanno.*", "").replaceAll("w.", "")
    toInt(id) -> write((s \ "w").map(getToken))
  }

  def batchInsert(s: Iterator[(Int, String)]): Unit = {


    def bindings(id: Int, json: String) = Seq(db.Binding("elan_annotatie_id", id), db.Binding("tokens", json))

    val insertStatement = s"insert into tagged_tokens (elan_annotatie_id, tokens) values (:elan_annotatie_id, :tokens)"

    Console.err.println(insertStatement)
    val batch = db.QueryBatch[(Int, String)](insertStatement, { case (x, y) => bindings(x, y) })
    batch.insert(s)
  }


  def getTokens(dir: String) = {

    val files = new java.io.File(dir).listFiles().filter(_.getName.endsWith(".xml")).iterator.map(XML.loadFile)
    val tokens = files.flatMap(f => (f \\ "s").map(getTokenArray).iterator)
    // tokens.foreach(x => println(write(x)))
    tokens
  }

  def main(args: Array[String]) = {
    db.runStatement("create table if not exists tagged_tokens (elan_annotatie_id integer, tokens text)")
    db.runStatement("create view elan_annotatie_plus as select elan_annotatie.*, tagged_tokens.tokens as tagged_tokens from elan_annotatie left join tagged_tokens on tagged_tokens.elan_annotatie_id=elan_annotatie.elan_annotatie_id;")
    db.runStatement("delete from tagged_tokens")
    val tokens = getTokens(dir)

    //tokens.foreach(println)
    batchInsert(tokens)
  }
}
