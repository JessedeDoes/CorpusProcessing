package corpusprocessing.GCND

import database.DatabaseUtilities.Select
import database._
import org.json4s._
import org.json4s.jackson.Serialization._

import scala.xml.PrettyPrinter
import corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.conll_u.{AlpinoSentence, AlpinoToken}

import java.io.PrintWriter
import scala.xml._
import corpusprocessing.GCND.Metadata.SimpleRelation
import GCNDDatabase.db

import scala.collection.mutable

object Metadata {

  type record = Map[String, Any]

  trait Relation {
     def name: String
     def map(m: record): Table

     def sourceTable : Table
     def targetTable: Table
  }

  case class SimpleRelation(name: String, sourceTable: Table, targetTable: Table, key_field: String, foreign_field: String) extends Relation {
    override def toString = s"Relation $name(${sourceTable.name}, ${targetTable.name}, ${sourceTable.name}.$key_field = ${targetTable.name}.$foreign_field)"

    def map(m: record): Table = {
      if (m.contains(key_field)) {
        this.targetTable.filter(foreign_field, m(key_field))
      } else this.targetTable.copy(data = List())
    }

    def x(r: SimpleRelation) = CrossTableRelation(name = this.name + ";  " + r.name, this, r)

    def converse =  SimpleRelation(name = this.targetTable.name + "X"  + this.sourceTable.name, targetTable, sourceTable, foreign_field, key_field)
  }

  case class CrossTableRelation(name: String, r1: SimpleRelation, r2: SimpleRelation) extends Relation {
    override def toString = s"CrossTableRelation(${r1.sourceTable.name}/${r1.targetTable.name}/${r2.targetTable.name}, ${r1.name}, ${r2.name})"

    val sourceTable = r1.sourceTable
    val targetTable = r2.targetTable
    def map(m: record): Table = {
      val t1 = r1.map(m)
      r2.targetTable.copy(data = t1.data.flatMap(m => r2.map(m).data))
    }
  }

  case class Table(data: List[record], name: String, id_field: String, skip_fields: Set[String]  = Set()) {

    def isCrossTable = name.contains("__")
    override  def toString = s"Table($name,key=$id_field)"
    lazy val size = data.size

    case class Index(field_name: String) {
      lazy val map: Map[Any, List[record]] = data.filter(_.contains(field_name)).map(m => m(field_name.toString) -> m).groupBy(_._1).mapValues(_.map(_._2))
      def apply(field_value: Any): List[record] = map.getOrElse(field_value, List())
    }

    lazy val indices: Map[String, Index] = data.flatMap(m => m.keySet).map(field => field -> Index(field)).toMap
    def filter(field_name: String, value: Any): Table  = {
      // Console.err.println(indices)

      val r = if (indices.contains(field_name)) indices(field_name)(value) else List()
      //val reference = this.data.filter(x => x(field_name) == value)
      //println(s"${r.size} ==== ${reference.size} ??? ")
      this.copy(data = r)
    }

    def filter(field_name: String, f: Any => Boolean) = this.copy(data = this.data.filter(x => f(x(field_name))))

    def makeXML(m: Map[String, Any], visited: Set[String]  = Set()): Elem = {
      // Console.err.println(s"Making XML for table $this")
      //val element_id = this.name + "." + m(this.id_field).toString //  xml:id={element_id} // Niet gebruikt dit???
      val e0: Elem = <elem/>.copy(label = this.name)

      val e1: Elem = <elem/>

      val noEmpty = true
      val children: Seq[Node] = m.filter({case (k,v) => !skip_fields.contains(k)}).flatMap({
        case (k, v: String) => e1.copy(label=k, child = Text("" + v))
        case (k, null) if (!noEmpty) =>  e1.copy(label=k)
        case _ => Seq()
      }).toSeq

      val relationCandidates = Schema.relations.filter(r => r.sourceTable.name == this.name)
      //Console.err.println(s"relation candidates: ${relationCandidates.map(_.name)}, followed = ${visited}")

      val foreignChildren = relationCandidates.filter(r => !visited.contains(this.name)).flatMap(r => {
        // Console.err.println(s"Following $r (followed (${visited.size}) = ${visited}")
        val foreignRecords = r.map(m).toXML(visited = visited ++ Set(this.name))
        val bla = <e rel={r.name.replaceAll("X","⟶")}/>
        foreignRecords.map(x => x.asInstanceOf[Elem].copy(attributes = bla.attributes))
      }).toSet.toSeq

      val newChildren = if (isCrossTable) foreignChildren else (children ++ foreignChildren)
      e0.copy(child = newChildren.sortBy(_.label))
    }

    def toXML(visited: Set[String]  = Set()) : NodeSeq = if (visited.contains(this.name)) Seq() else data.flatMap(m => makeXML(m, visited))
  }


   type dinges = List[Map[String,String]]
   val slurpMap: mutable.Map[String, dinges]  = new mutable.HashMap[String, dinges]()
   def cachedSlurp(tableName: String) : dinges= {
      if (slurpMap.contains(tableName))  {
         Console.err.println(s"Cached slurp: $tableName")
         slurpMap(tableName)
       } else {
         val x = db.slurp(db.allRecords(tableName))
         slurpMap(tableName) = x
         x
      }
   }

   lazy val pretty = new PrettyPrinter(100,4)
   def zlurp0(tableName: String, id_field_name: String, skip_fields: Set[String] = Set()): Table =
     Table(data = cachedSlurp(tableName), tableName, id_field_name, skip_fields)
   def zlurp(tableName: String, skip_fields: Set[String] = Set()): Table = {
     Console.err.println(s"slurping $tableName")
     zlurp0(tableName, tableName + "_id", skip_fields = skip_fields) }

   object Schema {

     val relationQuery = """SELECT distinct
                           |    tc.constraint_name,
                           |    tc.table_name,
                           |    kcu.column_name,
                           |    ccu.table_name AS foreign_table_name,
                           |    ccu.column_name AS foreign_column_name
                           |FROM information_schema.table_constraints AS tc
                           |JOIN information_schema.key_column_usage AS kcu
                           |    ON tc.constraint_name = kcu.constraint_name
                           |    AND tc.table_schema = kcu.table_schema
                           |JOIN information_schema.constraint_column_usage AS ccu
                           |    ON ccu.constraint_name = tc.constraint_name
                           |WHERE tc.constraint_type = 'FOREIGN KEY'
                           |    AND tc.table_schema='public'
                           |""".stripMargin

     db.runStatement(s"drop view inter_table_relations")
     db.runStatement(s"create view inter_table_relations as $relationQuery")


     lazy val alpino_annotatie: Table = zlurp("alpino_annotatie", skip_fields = Set("xml", "tokens"))
     lazy val elan_annotatie: Table = zlurp("elan_annotatie")



     lazy val opname: Table = zlurp("opname")
     lazy val opname__bestand: Table = zlurp("opname__bestand")

     lazy val persoon: Table = zlurp("persoon")
     lazy val persoon__beroep: Table = zlurp("persoon__beroep")
     lazy val persoon__woonplaats: Table = zlurp("persoon__woonplaats")
     lazy val persoon__beroepplaats: Table = zlurp("persoon__beroepplaats")
     lazy val persoon__schoolplaats: Table = zlurp("persoon__schoolplaats")
     lazy val persoon_vaderplaats: Table = zlurp("persoon_vaderplaats")
     lazy val persoon_moederplaats: Table = zlurp("persoon_moederplaats")
     lazy val persoon_partnerplaats: Table = zlurp("persoon_partnerplaats")


     lazy val opname__persoon: Table = zlurp("opname__persoon")
     lazy val opname_functie: Table = zlurp("opname_functie")

     lazy val plaats: Table = zlurp("plaats")
     lazy val provincie: Table = zlurp("provincie")
     lazy val land: Table = zlurp("land")
     lazy val dialectgebied: Table = zlurp("dialectgebied")
     lazy val dialectgebied_ruw: Table = zlurp("dialectgebied_ruw")
     lazy val beroep: Table = zlurp("beroep")
     lazy val mobiliteit: Table = zlurp("mobiliteit")
     lazy val bestand: Table = zlurp(tableName = "bestand")
     lazy val gender: Table = zlurp("gender")


     lazy val relatie: Table = zlurp("relatie")

     lazy val transcriptie: Table = zlurp("transcriptie")
     lazy val transcriptie__bestand: Table = zlurp("transcriptie__bestand")
     lazy val transcriptie__persoon: Table = zlurp("transcriptie__persoon")
     lazy val transcriptie_status: Table = zlurp("transcriptie_status")

     /*
     object auxiliaryRelations  {
       val r : Map[String, SimpleRelation] = List(

         SimpleRelation("persoonXpersoon__woonplaats", persoon, persoon__woonplaats, "persoon_id", "persoon_id"),
         SimpleRelation("persoon__woonplaatsXplaats", persoon__woonplaats, plaats, "plaats_id", "plaats_id"),
         SimpleRelation("persoonXpersoon__schoolplaats", persoon, persoon__schoolplaats, "persoon_id", "persoon_id"),
         SimpleRelation("persoon__schoolplaatsXplaats", persoon__schoolplaats, plaats, "plaats_id", "plaats_id"),
         SimpleRelation("persoonXpersoon__beroepplaats", persoon, persoon__beroepplaats, "persoon_id", "persoon_id"),
         SimpleRelation("persoon__beroepsplaatXplaats", persoon__beroepplaats, plaats, "plaats_id", "plaats_id"),
         SimpleRelation("persoonXpersoon__beroep", persoon, persoon__beroep, "persoon_id", "persoon_id"),
         SimpleRelation("persoon__beroepXberoep", persoon__beroep, beroep, "beroep_id", "beroep_id"),

         SimpleRelation("opnameXopname__persoon", opname, opname__persoon, "opname_id", "opname_id"),
         SimpleRelation("opname__persoonXpersoon", opname__persoon, persoon, "persoon_id", "persoon_id"),
         SimpleRelation("opnameXopname__bestand", opname, opname__bestand, "opname_id", "opname_id"),
         SimpleRelation("opname__bestandXbestand", opname__bestand, bestand, "bestand_id", "bestand_id"),
         SimpleRelation("opname__persoonXopname_functie", opname__persoon, opname_functie, "opname_functie_id", "opname_functie_id"),

         SimpleRelation("transcriptieXtranscriptie__bestand", transcriptie, transcriptie__bestand, "transcriptie_id", "transcriptie_id"),
         SimpleRelation("transcriptie__bestandXbestand", transcriptie__bestand, bestand, "bestand_id", "bestand_id"),
         SimpleRelation("transcriptieXtranscriptie__persoon", transcriptie, transcriptie__persoon, "transcriptie_id", "transcriptie_id"),
         SimpleRelation("transcriptie__persoonXpersoon", transcriptie__persoon, persoon, "persoon_id", "persoon_id"),
       ).map(r => r.name -> r).toMap

     }
     */

     object allRelations {
       val r = db.slurp(db.allRecords("inter_table_relations")).map(m =>
         SimpleRelation(
           name = s"${m("table_name")}X${m("foreign_table_name")}",
           sourceTable = zlurp(m("table_name")),
           targetTable = zlurp(m("foreign_table_name")),
           key_field = m("column_name"),
           foreign_field = m("foreign_column_name")
         )).flatMap(r => List(r, r.converse)).map(r => r.name -> r).toMap
       lazy val possibleCrosses: Seq[CrossTableRelation] = r.values.toSeq.flatMap(x => r.values.map(y => x -> y))
         .filter({case (x,y) => x.targetTable.name == y.sourceTable.name && (x.targetTable.name == x.sourceTable.name + "__" + y.targetTable.name)}).map({case (r1,r2) => r1 x r2})
         .filter(x => x.r1.targetTable.name != "opname__persoon")
         .sortBy(_.toString)
       r.values.map(_.toString).toList.sorted.foreach(println)
       possibleCrosses.foreach(x => println(x + s" link=${x.r1.targetTable.name}"))
     }

     import allRelations.r

     lazy val relations = List[Relation](
       SimpleRelation("alpino_annotatieXopname__persoon", alpino_annotatie, opname__persoon, "opname_persoon_id", "opname_persoon_id"),
       SimpleRelation("opname__persoonXpersoon", opname__persoon, persoon, "persoon_id", "persoon_id"),
       SimpleRelation("opname__persoonXopname_functie", opname__persoon, opname_functie, "opname_functie_id", "opname_functie_id"),
       r("elan_annotatieXopname__persoon"),
       r("opname__persoonXpersoon") ,
       r("opnameXopname__persoon"),
       r("opname__persoonXopname_functie"),
       // r("opnameXopname__persoon"),
       // persoon

       SimpleRelation("geboorteplaats", persoon, plaats, "geboorte_plaats_id", "plaats_id"),
       SimpleRelation("persoonXgender", persoon, gender, "gender_id", "gender_id"),
       SimpleRelation("persoonXwoonplaatsmobiliteit", persoon, mobiliteit, "woonplaats_mobiliteit_id", "mobiliteit_id"),
       SimpleRelation("persoonXberoepmobiliteit", persoon, mobiliteit, "beroep_mobilitiet_id", "mobiliteit_id"), // TODO: tiet nog tot teit corrigeren

       SimpleRelation("persoonXvader", persoon, persoon_vaderplaats, "persoon_id", "persoon_id2"),
       SimpleRelation("persoonXmoeder", persoon, persoon_moederplaats, "persoon_id", "persoon_id2"),
       SimpleRelation("persoonXpartner", persoon, persoon_partnerplaats, "persoon_id", "persoon_id2"),

       r("persoonXpersoon__woonplaats") x r("persoon__woonplaatsXplaats"),
       r("persoonXpersoon__schoolplaats") x r("persoon__schoolplaatsXplaats"),
       r("persoonXpersoon__beroepplaats") x r("persoon__beroepplaatsXplaats"),
       r("persoonXpersoon__beroep") x r("persoon__beroepXberoep"),
      // r("persoonXpersoon__persoon") x r("persoon__persoonXpersoon"),
      // opname
       SimpleRelation("opnameXplaats", opname, plaats, "plaats_id", "plaats_id"),


       // plaats
       SimpleRelation("plaatsXregio", plaats, provincie, "provincie_id", "provincie_id"),
       SimpleRelation("plaatsXland", plaats, land, "land_id", "land_id"),
       SimpleRelation("plaatsXdialectgebied", plaats, dialectgebied, "dialectgebied_id", "dialectgebied_id"),
       SimpleRelation("dialectgebiedXdialectgebiedruw", dialectgebied, dialectgebied_ruw, "dialectgebied_ruw_id", "dialectgebied_ruw_id"),
       // opname


       // transcriptie
       SimpleRelation("transcriptieXopname", transcriptie, opname, "opname_id", "opname_id"),
       SimpleRelation("transcriptieXstatus", transcriptie, transcriptie_status, "transcriptie_status_id", "transcriptie_status_id")
     ) ++ allRelations.possibleCrosses
   }

   import Schema._

  /*
   lazy val alle_gebruikte_personen = {
     val idz = alpinos.map(_.opname_persoon_id.toString).toSet
     val o_peetjes = opname__persoon.filter("opname__persoon_id", x => idz.contains(x))
     o_peetjes
   }
*/
   def getMetadata(a: AlpinoAnnotation) = {
     val opname_persoon_id = a.opname_persoon_id.toString;
     val t0 = opname__persoon.filter("opname__persoon_id", opname_persoon_id)
     val info = <alignment_info>{a.n_m}</alignment_info>
     info +: t0.toXML()
   }

   val scope = <x xmlns="http://gcnd.ivdnt.org/metadata" xmlns:gcndmeta="http://gcnd.ivdnt.org/metadata"></x>.scope
   def getMetadata(transcription: Transcription)  = {
     val transcriptie_id = transcription.transcriptie_id
     val t0: Table = transcriptie.filter("transcriptie_id", transcriptie_id.toString)
     val hasAlpino = transcription.elanAnnotations.exists(_.useAlpino)
     val z = <gcnd_transcriptie_metadata xml:id={"gcnd.metadata." + transcriptie_id}>
       {t0.toXML()}
       <hasAlpino>{hasAlpino}</hasAlpino>
       </gcnd_transcriptie_metadata>.copy(scope=scope)
     z
   }

  def getMetadataForElanAnnotation(elan_annotatie_id: Int): Elem = {
    val t0 = elan_annotatie.filter("elan_annotatie_id", elan_annotatie_id.toString)
    val xml = t0.toXML()

    val persoon_id = (xml \\ "persoon_id").text
    val functie = xml \\ "opname_functie" \\ "label"
    val alias = (xml \\ "alias").text
    val persoontje = <persoon ref={"#" + persoon_id}>
      <naam>{xml \\ "familienaam"}, {xml \\ "voornaam"}</naam>
      <alias>{alias}</alias>
      <functie><label>{functie.text}</label></functie>
    </persoon>
    val z = <gcnd_annotatie_metadata xmlns="http://gcnd.ivdnt.org/metadata" xmlns:gcndmeta="http://gcnd.ivdnt.org/metadata">{persoontje}</gcnd_annotatie_metadata>
    // Console.err.println(z)
    z
  }

  def main(args: Array[String]): Unit = {
    /*
    alpinos.take(1).foreach(a => {

      val m = <meta>{getMetadata(a)}</meta>
      println(pretty.format(m))
    })
    */

    val transcriptieMeta = <meta>{transcriptie.toXML()}</meta>;
    println(pretty.format(transcriptieMeta))
  }
}


object listMetadataFromBlacklabServer {
  val url = "http://svotmc10.ivdnt.loc:8080/blacklab-server/GCND/"
  lazy val response = XML.load(url)
  lazy val metadataFields = (response \\ "metadataField")
  lazy val fieldNames = metadataFields.map(x => (x \ "@name").text).toSet.toList.sorted
  def pad(x: String,n: Int) =  " " * (n - x.size)
  def main(args: Array[String])  = {
     fieldNames.foreach(name => {
       val s = s"['$name'${pad(name,30)}               ,      x       ,       x       ,                      ,                      ,                ],"
       println(s)
     })
  }
}
/*
Table "public.opname__persoon"
Column        |       Type        | Collation | Nullable |                           Default
---------------------+-------------------+-----------+----------+-------------------------------------------------------------
opname__persoon_id  | integer           |           | not null | nextval('opname__persoon_opname__persoon_id_seq'::regclass)
persoon_id          | integer           |           | not null |
opname_id           | integer           |           | not null |
opname_functie_id   | integer           |           | not null |
nummer              | integer           |           |          |
opname_persoon_code | character varying |           |          |


Table "public.beroep"
Column   |          Type          | Collation | Nullable |                  Default
-----------+------------------------+-----------+----------+-------------------------------------------
beroep_id | integer                |           | not null | nextval('beroep_beroep_id_seq'::regclass)
label     | character varying(250) |           |          |
Indexes:

Table "public.bestand"
Column   |          Type           | Collation | Nullable |                   Default
------------+-------------------------+-----------+----------+---------------------------------------------
bestand_id | integer                 |           | not null | nextval('bestand_bestand_id_seq'::regclass)
naam       | character varying       |           | not null |
pad        | character varying(1024) |           | not null |

Table "public.gender"
Column   |          Type          | Collation | Nullable |                  Default
-----------+------------------------+-----------+----------+-------------------------------------------
gender_id | integer                |           | not null | nextval('gender_gender_id_seq'::regclass)
label     | character varying(250) |           |          |
Indexes:
"pk_gender" PRIMARY KEY, btree (gender_id)
"gender_name_index" UNIQUE, btree (label)

Table "public.land"
Column  |          Type          | Collation | Nullable |                Default
---------+------------------------+-----------+----------+---------------------------------------
land_id | integer                |           | not null | nextval('land_land_id_seq'::regclass)
label   | character varying(250) |           |          |

Table "public.opname"
Column    |       Type        | Collation | Nullable |                  Default
-------------+-------------------+-----------+----------+-------------------------------------------
opname_id   | integer           |           | not null | nextval('opname_opname_id_seq'::regclass)
opname_code | character varying |           | not null |
nummer      | integer           |           | not null |
plaats_id   | integer           |           |          |
jaar        | integer           |           |          |
maand       | integer           |           |          |
dag         | integer           |           |          |
tijdsduur   | integer           |           |          |

Table "public.persoon"
Column       |       Type        | Collation | Nullable |                   Default
--------------------+-------------------+-----------+----------+---------------------------------------------
persoon_id         | integer           |           | not null | nextval('persoon_persoon_id_seq'::regclass)
voornaam           | character varying |           |          |
familienaam        | character varying |           |          |
alias              | character varying |           |          |
geboorte_jaar      | integer           |           |          |
geboorte_maand     | integer           |           |          |
geboorte_dag       | integer           |           |          |
geboorte_plaats_id | integer           |           |          |
gender_id          | integer           |           |          |
schoolleeftijd     | integer           |           |          |
opleiding          | character varying |           |          |

Table "public.plaats"
Column    |       Type        | Collation | Nullable |                  Default
-------------+-------------------+-----------+----------+-------------------------------------------
plaats_id   | integer           |           | not null | nextval('plaats_plaats_id_seq'::regclass)
kloeke_code | character varying |           | not null |
naam        | character varying |           |          |
land_id     | integer           |           |          |
regio_id    | integer           |           |          |

Table "public.regio"
Column  |          Type          | Collation | Nullable |                 Default
----------+------------------------+-----------+----------+-----------------------------------------
regio_id | integer                |           | not null | nextval('regio_regio_id_seq'::regclass)
label    | character varying(250) |           |          |
Indexes:
  "pk_regio" PRIMARY KEY, btree (regio_id)
  "regio_name_index" UNIQUE, btree (label)

Table "public.relatie"
Column   |          Type          | Collation | Nullable |                   Default
------------+------------------------+-----------+----------+---------------------------------------------
relatie_id | integer                |           | not null | nextval('relatie_relatie_id_seq'::regclass)
label      | character varying(250) |           |          |
Indexes:

Table "public.transcriptie"
Column        |  Type   | Collation | Nullable |                        Default
----------------------+---------+-----------+----------+-------------------------------------------------------
transcriptie_id      | integer |           | not null | nextval('transcriptie_transcriptie_id_seq'::regclass)
opname_id            | integer |           | not null |
transcriptie_type_id | integer |           | not null |
jaar                 | integer |           |          |


 */