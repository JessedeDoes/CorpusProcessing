package corpusprocessing.GCND

import database.DatabaseUtilities.Select
import database._
import org.json4s._
import org.json4s.jackson.Serialization._

import scala.xml.PrettyPrinter
import corpusprocessing.clariah_training_corpora.moderne_tagging.lassy.conll_u.{AlpinoSentence, AlpinoToken}

import java.io.PrintWriter
import scala.xml._
import GCNDDatabase.{Token, alpinos, db, elans}
import corpusprocessing.GCND.Metadata.Relation
object Metadata {

  type record = Map[String, Any]

  case class Table(data: List[record], name: String, id_field: String, skip_fields: Set[String]  = Set()) {

    def isCrossTable = name.contains("__")
    override  def toString = s"Table($name,key=$id_field)"
    lazy val size = data.size
    def filter(field_name: String, value: Any): Table  = {
      this.copy(data = this.data.filter(x => x(field_name) == value))
    }

    def filter(field_name: String, f: Any => Boolean) = this.copy(data = this.data.filter(x => f(x(field_name))))

    def makeXML(m: Map[String, Any]): Elem = {
      Console.err.println(s"Making XML for table $this")
      val element_id = this.name + "." + m(this.id_field).toString //  xml:id={element_id}
      val e0: Elem = <elem/>.copy(label = this.name)

      val e1: Elem = <elem/>
      val children = m.filter({case (k,v) => !skip_fields.contains(k)}).map({
        case (k, v: String) => e1.copy(label=k, child = Text("" + v))
        case (k, null) =>  e1.copy(label=k)
      }).toSeq

      val foreignChildren: Seq[Node] = Tables.relations.filter(r => r.t1.name == this.name).flatMap(r => {
         if (m.contains(r.key_field)) {
           Console.err.println(s"Join on $r with key= ${r.key_field}")
           val foreignRecords = r.t2.filter(r.foreign_field, m(r.key_field)).makeXML()
           Console.err.println(s"Foreign record: ${foreignRecords.size}")
           val bla = <e rel={r.name}/>
           foreignRecords.map(x => x.asInstanceOf[Elem].copy(attributes = bla.attributes))
         } else  {
           Console.err.println(s"${r.key_field} NOT in keySet ${m.keySet} for relation $r")
           Seq()
        }}).toSet.toSeq
      val newChildren = if (isCrossTable) foreignChildren else (children ++ foreignChildren)
      e0.copy(child = newChildren)
    }

    def makeXML() : NodeSeq = data.map(makeXML)
  }

  case class Relation(name: String, t1: Table, t2: Table, key_field: String, foreign_field: String) {
    override def toString = s"Relation $name(${t1.name}, ${t2.name}, ${t1.name}.$key_field = ${t2.name}.$foreign_field)"
  }
  
   lazy val pretty = new PrettyPrinter(100,4)
   def zlurp0(tableName: String, id_field_name: String, skip_fields: Set[String] = Set()): Table = Table(data = db.slurp(db.allRecords(tableName)), tableName, id_field_name, skip_fields)
   def zlurp(tableName: String, skip_fields: Set[String] = Set()): Table = zlurp0(tableName, tableName + "_id", skip_fields = skip_fields)

   object Tables {
     lazy val alpino_annotatie: Table = zlurp("alpino_annotatie", skip_fields = Set("xml", "tokens"))
     lazy val elan_annotatie: Table = zlurp("elan_annotatie")



     lazy val opname: Table = zlurp("opname")
     lazy val opname__bestand: Table = zlurp("opname__bestand")

     lazy val persoon: Table = zlurp("persoon")
     lazy val persoon__beroep: Table = zlurp("persoon__beroep")
     lazy val persoon__woonplaats: Table = zlurp("persoon__woonplaats")
     lazy val persoon__beroepsplaats: Table = zlurp("persoon__beroepplaats")
     lazy val opname__persoon: Table = zlurp("opname__persoon")
     lazy val opname_functie: Table = zlurp("opname_functie")

     lazy val plaats: Table = zlurp("plaats")
     lazy val regio: Table = zlurp("regio")
     lazy val land: Table = zlurp("land")
     lazy val beroep: Table = zlurp("beroep")
     lazy val bestand: Table = zlurp(tableName = "bestand")
     lazy val gender: Table = zlurp("gender")


     lazy val relatie: Table = zlurp("relatie")

     lazy val transcriptie: Table = zlurp("transcriptie")
     lazy val transcriptie__bestand: Table = zlurp("transcriptie__bestand")
     lazy val transcriptie__persoon: Table = zlurp("transcriptie__persoon")



     lazy val relations = List[Relation](
       Relation("alpino_annotatieXopname__persoon", alpino_annotatie, opname__persoon, "opname_persoon_id", "opname_persoon_id"),

       // persoon
       Relation("persoonXwoonplaats", persoon, persoon__woonplaats, "persoon_id", "persoon_id"),

       Relation("personXgeboorteplaats", persoon, plaats, "geboorte_plaats_id", "plaats_id"),
       Relation("persoonXgender", persoon, gender, "gender_id", "gender_id"),


       Relation("persoonXpersoon__beroepsplaats", persoon, persoon__beroepsplaats, "persoon_id", "persoon_id"),
       Relation("persoon__beroepsplaatsXplaats", persoon__beroepsplaats, plaats, "plaats_id", "plaats_id"),

       Relation("persoonXpersoon__beroep", persoon, persoon__beroep, "persoon_id", "persoon_id"),
       Relation("persoon__beroepXberoep", persoon__beroep, beroep, "beroep_id", "beroep_id"),
       // plaats
       Relation("plaatsXregio", plaats, regio, "regio_id", "regio_id"),
       Relation("plaatsXland", plaats, land, "land_id", "land_id"),
       // opname
       Relation("opnameXopname__persoon", opname, opname__persoon, "opname_id", "opname_id"),
       Relation("opname__persoonXpersoon", opname__persoon, persoon, "persoon_id", "persoon_id"),

       Relation("opnameXopname__bestand", opname, opname__bestand, "opname_id", "opname_id"),
       Relation("opname__bestandXbestand", opname__bestand, bestand, "bestand_id", "bestand_id"),

       Relation("opname__persoonXopname_functie", opname__persoon, opname_functie, "opname_functie_id", "opname_functie_id"),
       Relation("opnameXplaats", opname, plaats, "plaats_id", "plaats_id"),

       // transcriptie
       Relation("transcriptieXopname", transcriptie, opname, "opname_id", "opname_id"),
       Relation("transcriptieXtranscriptie__bestand", transcriptie, transcriptie__bestand, "transcriptie_id", "transcriptie_id"),
       Relation("transcriptie__bestandXbestand", transcriptie__bestand, bestand, "bestand_id", "bestand_id"),

       Relation("transcriptieXtranscriptie__persoon", transcriptie, transcriptie__persoon, "transcriptie_id", "transcriptie_id"),
       Relation("transcriptie__persoonXpersoon", transcriptie__persoon, persoon, "persoon_id", "persoon_id"),

       Relation("woonplaatsXplaats", persoon__woonplaats, plaats, "plaats_id", "plaats_id"),
     )
   }

   import Tables._

   lazy val alle_gebruikte_personen = {
     val idz = alpinos.map(_.opname_persoon_id.toString).toSet
     val o_peetjes = opname__persoon.filter("opname__persoon_id", x => idz.contains(x))
     o_peetjes
   }

   def getMetadata(a: AlpinoAnnotation) = {
     val opname_persoon_id = a.opname_persoon_id.toString;
     val t0 = opname__persoon.filter("opname__persoon_id", opname_persoon_id)
     val info = <alignment_info>{a.n_m}</alignment_info>
     info +: t0.makeXML()
   }

   val scope = <x xmlns="http://gcnd.ivdnt.org/metadata" xmlns:gcndmeta="http://gcnd.ivdnt.org/metadata"></x>.scope
   def getMetadata(transcriptie_id: Int)  = {
     val t0 = transcriptie.filter("transcriptie_id", transcriptie_id.toString)
     val z = <gcnd_metadata xml:id={"gcnd.metadata." + transcriptie_id}>{t0.makeXML()}</gcnd_metadata>.copy(scope=scope)
     z
   }

  def main(args: Array[String]): Unit = {
    alpinos.take(1).foreach(a => {

      val m = <meta>{getMetadata(a)}</meta>
      println(pretty.format(m))
    })
    val transcriptieMeta = <meta>{transcriptie.makeXML()}</meta>;
    println(pretty.format(transcriptieMeta))
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