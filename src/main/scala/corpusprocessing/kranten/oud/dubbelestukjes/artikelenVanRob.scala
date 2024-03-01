package corpusprocessing.kranten.oud.dubbelestukjes
import java.io.{FileReader, PrintWriter}
import org.apache.commons.csv._

import scala.collection.JavaConverters._
object artikelenVanRob {
  val dir = "/mnt/Projecten/Corpora/Historische_Corpora/Courantencorpus/CSVBestandenNicoline/"
  val artikelenVanRob = dir + "artikelenVanRobUitSql_feb_2019.csv"
  val compleetMaart = dir + "compleet krantenbestand mrt 2019.csv"
  val getMe = "ddd:010411954:mpeg21:p001:a0001"
  val getMeToo = "ddd:010411965:mpeg21:a0001"
  def loadRobStyle(path: String) =  {
    val in = new FileReader(path)
    val b = CSVFormat.DEFAULT.builder()
    b.setDelimiter(";")
    b.setRecordSeparator("\r\n")
    b.setHeader("id;article_id;paper_id;paper_title;date;identifier;article;article_title;header;subheader;article_text;err_text_type;comments_corrector;colophon;colophon_text;tekstsoort;plaats;land"
      .split(";"):_*)
    val f = b.build()
    val records = f.parse(in).asScala
    val ids = records.take(10000).map(r => {

      val id = r.get("identifier")
      val other_id = id.replaceAll(":p[0-9]+", "")
      if (other_id == getMeToo)
        println(r.get("identifier") -> r.get("id") -> r.get("article_text").replaceAll("\\n", " "))

      id
    })
    val counts = ids.groupBy(identity).mapValues(_.size)
    println(counts.filter(_._2 > 4))
  }


  val fieldsForComplete = Seq(
    "x0",
    "x1",
    "id",
    "article_id",
    "article",
    "article_zonder_subheader",
    "header_0",
    "header_1",
    "f1",
    "f2",
    "f3",
    "f4",
    "date",
    "f5",
    "header_3",
    "header_4",
    "n1",
    "f6",
    "n2",
    "t1",
    "t2",
    "f7",
    "f8",
    "subheader",
    "soort",
    "f11",
    "datum_bewerkt",
    "bewerker",
  )
  def loadCompleteStyle(path:  String) = {
    val in = new FileReader(path)
    val b = CSVFormat.DEFAULT.builder()
    b.setDelimiter(",")
    //b.setRecordSeparator("\n")
    b.setHeader("x0;x1;id;article_id;article;article_zonder_subheader;header_0;header_1;f1;f2;f3;f4;date;f5;header_3;header_4;n1;f6;n2;t1;t2;f7;f8;subheader;soort;f11;datum_bewerkt;bewerker"
      .split(";"): _*)
    val f = b.build()
    val records = f.parse(in).asScala
    val ids = records.take(1000).map(r => {
      val id = r.get("article_id")
      if (id == getMeToo)
        println(s"${r.get("article_id")}\t${r.get("article").take(80)}")
      val other_id = id.replaceAll(":p[0-9]+", "")
      id
    })
    val counts = ids.groupBy(identity).mapValues(_.size)
    println(counts.filter(_._2 > 4))
  }

  def cleanField(s: String)  = {
    s.replaceAll("\\n", "<lb/> ").replaceAll("\\v", "<vtab/> ").replaceAll("\\s+", " ").replaceAll("\\\\", "<backslash/>").trim
  }
  def toTSV(path: String, nColumns: Option[Int]  = None) = {
    val in = new FileReader(path)
    val b = CSVFormat.DEFAULT.builder()
    b.setDelimiter(",")
    //b.setRecordSeparator("\n")

    val f = b.build()
    val records  = f.parse(in).asScala.iterator
    val TSVLines = records
      .filter(r => nColumns.forall(x => {
         if (r.size() != x) Console.err.println(s"Wrong size: ${r.size()}");
         r.size() == x})
      )
      .map(r => {r.values().toList.map(cleanField).mkString("\t")})
    // println("piep")
    val out = new PrintWriter(path.replaceAll(if (path.endsWith(".csv")) ".csv$" else "$", ".tsv"))
    TSVLines.foreach(x => out.println(x))
    out.close
  }

  def testje() = {
    val pw = new java.io.PrintWriter(dir + "geenControlK.csv")
    val lines = io.Source.fromFile(compleetMaart).getLines.map(x => x.replaceAll("\\v", "<NEWLINE>"))
    lines.take(500).foreach(pw.println)
    pw.close()
  }
  def main(args: Array[String])  = {
    // testje()
    ///loadRobStyle(artikelenVanRob)
    toTSV(compleetMaart, Some(28))
  }
}

object Bla {
  val h  = """x0;
  x1;
  id;
  article_id;
  article;
  article_zonder_subheader;
  header_0;
  header_1;
  f1;
  f2;
  f3;
  f4;
  date;
  f5;
  header_3;
  header_4;
  n1;
  f6;
  n2;
  t1;
  t2;
  f7;
  f8;
  subheader;
  soort;
  f11;
  datum_bewerkt; bewerker"""
  val h0 = h.split("\\s*;\\s*")
  def main(args: Array[String]) = {
   h0.foreach(x => { println(s"""  "$x text", """ ) })
   }
}
/*
Reader in = new FileReader("path/to/file.csv");
Iterable<CSVRecord> records = CSVFormat.RFC4180.parse(in);
for (CSVRecord record : records) {
    String columnOne = record.get(0);
    String columnTwo = record.get(1);
}

// "id";"article_id";"paper_id";"paper_title";"date";"identifier";"article";"article_title";"header";"subheader";"article_text";"err_text_type";"comments_corrector";"colophon";"colophon_text";"tekstsoort";"plaats";"land"
 */