package zeeuws

import database.Configuration

import scala.xml._
import utils.PostProcessXML._

object Classes {

  // Database classes

  case class Lemma(sense_id: String, original: String, an: String, definition: String)
  case class Keyword(lemma_id: String, keyword: String, kw_id: String) {
    lazy val id = kw_id
  }

  case class Response(keyword_id: String, keyword: String, place: String, isRegion: Boolean)
  // XML-based classes
  case class Entry(e: Node) {
    val entryLevelFields = List("volgnr", "import", "orig_trefwoord", "vernederl_trefwoord", "an_trefwoord").map(f => f -> (e \ f).text).toMap
    val headword = entryLevelFields("orig_trefwoord")
    val lemma = entryLevelFields("an_trefwoord")
    val senses = (e \\ "sense").map(s => Sense(s, this))
  }


  case class Sense(s: Node, e: Entry) {
    val volgnr = (s \ "@nr").text
    val id = e.entryLevelFields("volgnr") + "." + volgnr
    val definition = (s \ "definition").text
    val variants = (s \ "var").map(v => variant(v, this))
    val usages = (s \ "usg").map(u => usage(u, this)) ++ variants.flatMap(v => v.usages)
    val keywords = usages.map(_.keyword).toSet.zipWithIndex.map({case (w,i) => Keyword(id, w, s"$id.$i")})
    val kwMap = keywords.map(k => k.keyword -> k).toMap
    lazy val lemma = Lemma(id, e.headword, e.lemma, definition)
    lazy val attestations = usages.flatMap(_.attestations)
  }

  case class variant(v: Node, s: Sense) {
    val keyword = (v \ "v").text

    def usages = (v \ "usg").map(u => usage(u, s, Some(this)))
  }

  case class Attestation(s: Sense, u: usage, p: place) {
    val e = s.e
    val sense_id = s.id
    val keywordText = u.keyword
    lazy val keyword = s.kwMap(keywordText)
    override def toString() = s"$sense_id\t$keywordText\t${p.name}"
    lazy val response = Response(keywordText, keyword.id, p.name, p.isRegion) //
  }

  case class usage(u: Node, s: Sense, variant: Option[variant] = None) {
    val keyword = variant.map(_.keyword).getOrElse(s.e.entryLevelFields("orig_trefwoord"))
    val places = (u.child.filter(x => Set("region", "placeName").contains(x.label) && !((x \ "@use").text == "false"))).map(place)
    val attestations = places.map(p => Attestation(s, this, p))
  }


  case class place(p: Node) {
    val name = p.text
    val isRegion = p.label == "region"
  }
}

object Regions {
  val r0 = Map("W." -> "Walcheren",
    "de Westhoek van Sch." -> "Weetikveel",
    "West-Fl." -> "West-Flakkee",
    "grensstreek" -> "grensstreek",
    "Z. dl." -> "zuidelijk deel van WZO",
    "Z.B." -> "Zuid-Beveland",
    "Z.dl" -> "zuidelijk deel van WZO",
    "z.dl. van Z.V.O." -> "zuidelijk deel van WZO",
    "Z.eil" -> "Zeeuwse eilanden",
    "Z.eil." -> "Zeeuwse eilanden",
    "z.eil.beh.w." -> "Zeeuwse eilanden behalve Walcheren",
    "Z.V." -> "Zeeuws Vlaanderen (niet zeker of dit voorkomt)",
    "Z.V.O" -> "Oost-Zeeuws-Vlaanderen",
    "Z.V.O." -> "Oost-Zeeuws-Vlaanderen",
    "Z.V.W en O." -> "Oost en West-Zeeuws-Vlaanderen",
    "Z.V.W." -> "West-Zeeuws-Vlaanderen",
    "Zvo-zd" -> "zuidelijk deel van WZO").map({case (k,v) =>k.toLowerCase().trim -> v})

  val r2 = r0 ++ r0.map({case (k,v) =>k.replaceAll(" ", "") -> v})

  def apply(p: String): String = {
    val p1 = p.trim.toLowerCase().replaceAll(" ", "")
    r2.getOrElse(p1, "")
  }

  def contains(p: String): Boolean = {
    val p1 = p.trim.toLowerCase().replaceAll(" ", "")
    // if (p1.contains("eil")) println("Check: " + p1  + " " + r2.contains(p1))
    r2.contains(p1)
  }
  // println("keys for region map" + r2.keySet)
}

object zeeuws_xml_to_db {
   import Classes._
   val filename = "/mnt/Projecten/Hercules/DSDD/WZD/Data/definitieve bestanden/selectie_werkfile_definitief_met_var.xml"
   lazy val doc = XML.load(filename)
   val c = new Configuration(name="zeelandia", server="svowdb20.ivdnt.loc", user="postgres", password="inl", database = "wzd")
   lazy val db = new database.Database(c)

   def markUselessRegions(s: Elem): Elem = {
     val children = s.child.zipWithIndex

     val nc = children.map({case (c,i) =>
       c match {
         case e: Elem if e.label == "region" =>
           val after = children.drop(i+1).map(_._1).text.trim
             // println("####\n" + e)
             // println(after)
           if (after.startsWith("(")) {
             // System.exit(1)
             <region use="false" full={(e \ "@full").text}>{e.child}</region>
           } else
             e
         case _ => c
       }
     })
     s.copy(child = nc)
   }

  def markRegio(p: Elem): Elem = {
    val txt = p.text.toLowerCase().trim;
    if (Regions.contains(txt)) {
      <region full={Regions(txt)}>{txt}</region>
    } else p
  }

  lazy val postprocessedDoc = {
    val r = updateElement(doc, _.label == "placeName", markRegio)
    updateElement(r, _.label=="usg", markUselessRegions)
  }



  def extractFromEntries(): Unit = {
    val entries = (postprocessedDoc \\ "entry").map(Entry)
    val senses = entries.flatMap(_.senses)
    val lemmata = senses.map(_.lemma)
    val attestations = senses.flatMap(_.attestations)
    val responses = attestations.map(_.response)
    val keywords = senses.flatMap(_.keywords)

    //lemmata.foreach(println)
    // responses.foreach(println)

    /*
    val b = db.QueryBatch[Lemma]("insert into lievevrouw.articles_x (volgnr, trefwoord, xhtml) VALUES(:volgnr, :trefwoord, cast(:xhtml as xml))",
          l => Seq(db.Binding("volgnr", l.volgnr), db.Binding("trefwoord", l.orig_trefwoord), db.Binding("xhtml", l.patched_html)))

          case class Lemma(sense_id: String, original: String, an: String, definition: String)
     */

    // lemmata

    //db.runStatement("drop table if exists lemmata")
    //db.runStatement("create table lemmata (sense_id text, original text, an text, definition text)")

    val fieldMappingLemmata = Seq[(String, Lemma => String)](
      ("sense_id", l => l.sense_id),
      ("original", l => l.original),
      ("an", l => l.an),
      ("definition", l => l.definition)
    )

    insert[Lemma]("lemmata", fieldMappingLemmata, lemmata)

    // keywords Keyword(lemma_id: String, keyword: String, kw_id: String)

    val fieldMappingKeywords = Seq[(String, Keyword => String)](
      ("lemma_id", r => r.lemma_id),
      ("keyword", r => r.keyword),
      ("keyword_id", r => r.kw_id)
    )

    insert[Keyword]("keywords", fieldMappingKeywords, keywords)

    // responses   case class Response(keyword_id: String, keyword: String, place: String, isRegion: Boolean)

    val fieldMappingResponses  = Seq[(String, Response => String)](
      ("keyword_id", r => r.keyword_id),
      ("keyword", r => r.keyword),
      ("place", r => r.place),
      ("isRegion", r => r.isRegion.toString)
    )
    insert[Response]("responses", fieldMappingResponses, responses)


  }

  def insert[T](tableName: String, fieldMapping: Seq[(String, T => String)], things: Seq[T], recreate: Boolean = true) = {
    if (recreate)
    {
       db.runStatement(s"drop table if exists $tableName")
      val fieldDef = fieldMapping.map(_._1).map(s => s"$s text").mkString(", ")
       db.runStatement(s"create table $tableName ($fieldDef)")
    }
    val fields = fieldMapping.map(_._1)
    def bindings(l: T) = fieldMapping.map({case (n,f) => db.Binding(n, f(l))})
    val query = s"insert into $tableName (${fields.mkString(", ")}) values (${fields.map(":" + _).mkString(", ")})"
    val z = db.QueryBatch(query, bindings)
    z.insert(things)
  }

  def main(args: Array[String]) = {

    println("Gemarkeerd als regio:")
    (postprocessedDoc \\ "region").map(_.text).groupBy(x=>x).mapValues(_.size).toList.sorted.foreach(println)

    println("Niet gemarkeerd als regio:")
    (postprocessedDoc \\ "placeName").map(_.text).groupBy(x=>x).mapValues(_.size).toList.sorted.foreach(println)

    XML.save("/tmp/marked.xml", postprocessedDoc)

    extractFromEntries()

    // println(postprocessedDoc)
  }
}


object Sample {
  val sample =  <entry>
    <volgnr>263662</volgnr>
    <import>455</import>
    <orig_trefwoord>weiëblommetje, weiblommetje</orig_trefwoord>
    <vernederl_trefwoord>weiëbloemetje, weibloemetje</vernederl_trefwoord>
    <an_trefwoord>madeliefje, dubbele madelief, Bellis perennis</an_trefwoord>
    <locatie/>
    <illustratie/>
    <orig_tekst>
      <lemma>
        <orth>wei(ë)blommetje</orth>
      </lemma>
      <senses>
        <sense nr="1" reliability="">
          <definition>madeliefje (Bella perennis)</definition>
          :
          <usg>
            <placeName>W.</placeName>
            ;
            <placeName>Z.B.</placeName>
            ;
            <placeName>N.B.</placeName>
            (
            <placeName>Kam.</placeName>
            );
            <placeName>T.</placeName>
            (
            <placeName>Tln.</placeName>
            );
            <placeName>Phi.</placeName>
            ;
            <placeName>Sch-D</placeName>
            (
            <placeName>Zr.</placeName>
            ;
            <placeName>Kwv.</placeName>
            ;
            <placeName>Ng.</placeName>
            ;
            <placeName>Zn.</placeName>
            ;
            <placeName>Bh.</placeName>
            ;
            <placeName>Ow.</placeName>
            );
            <placeName>Z.V.W.</placeName>
            ;
            <placeName>L.v.Ax.</placeName>
            ;
            <placeName>Wdo.</placeName>
          </usg> <xr>Zie
          <ref type="lemma">koeieblomme(tie)</ref>
          ;
          <ref type="lemma">meiblommetje</ref>
        </xr>
          .
        </sense>
      </senses>
    </orig_tekst>
  </entry>
}

/*
W.	Walcheren
West-Fl.	West-Flakkee
Z. dl.	zuidelijk deel van WZO
Z.B.	Zuid-Beveland
Z.dl	zuidelijk deel van WZO
z.dl. van Z.V.O.	zuidelijk deel van WZO
Z.eil	Zeeuwse eilanden
Z.V.	Zeeuws Vlaanderen (niet zeker of dit voorkomt)
Z.V.O	Oost-Zeeuws-Vlaanderen
Z.V.W en O.	Oost en West-Zeeuws-Vlaanderen
Z.V.W.	West-Zeeuws-Vlaanderen
Zvo-zd	zuidelijk deel van WZO
 */