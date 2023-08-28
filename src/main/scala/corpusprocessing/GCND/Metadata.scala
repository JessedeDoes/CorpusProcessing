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

  case class Table(data: List[record], name: String, id_field: String) {


    lazy val size = data.size
    def filter(field_name: String, value: Any): Table  = {
      this.copy(data = this.data.filter(x => x(field_name) == value))
    }

    def filter(field_name: String, f: Any => Boolean) = this.copy(data = this.data.filter(x => f(x(field_name))))

    def makeXML(m: Map[String, Any]): Elem = {
      val e0: Elem = <elem xml:id={m(this.id_field).toString}/>.copy(label = this.name)
      val e1: Elem = <elem/>
      val children = m.map({
        case (k, v: String) => e1.copy(label=k, child = Text("" + v))
        case (k, null) =>  e1.copy(label=k)
      }).toSeq
      val foreignChildren: Seq[Node] = relations.filter(r => r.t1.name == this.name).flatMap(r => {
          val foreignRecords = r.t2.filter(r.foreign_field, m(r.key_field)).copy(name=r.name).makeXML()
          foreignRecords
        })
      e0.copy(child = children ++ foreignChildren)
    }

    def makeXML() : NodeSeq = data.map(makeXML)
  }

  case class Relation(name: String, t1: Table, t2: Table, key_field: String, foreign_field: String)

  val relations = List[Relation](

    Relation("persoon", opname__persoon, persoon, "persoon_id", "persoon_id"),
    Relation(name = "woonplaats", persoon, persoon__woonplaats, "persoon_id", "persoon_id"),
    Relation("geboorteplaats", persoon, plaats, "geboorte_plaats_id", "plaats_id"),
    Relation("gender", persoon, gender, "gender_id", "gender_id"),
    Relation("plaats", persoon__woonplaats, plaats, "plaats_id", "plaats_id"),
    Relation(name = "beroepsplaats", persoon, persoon__beroepsplaats, "persoon_id", "persoon_id"),
    Relation("plaats", persoon__beroepsplaats, plaats, "plaats_id", "plaats_id"),

    Relation("opname", transcriptie, opname, "opname_id", "opname_id"),
    Relation("plaats", opname, plaats, "plaats_id", "plaats_id")
  )


  lazy val pretty = new PrettyPrinter(100,4)
   def zlurp(tableName: String, id_field_name: String): Table = Table(data = db.slurp(db.allRecords(tableName)), tableName, id_field_name)
   def zlurp(tableName: String): Table = zlurp(tableName, tableName + "_id")

   lazy val beroep = zlurp("beroep")
   lazy val bestand= zlurp(tableName = "bestand")
   lazy val gender = zlurp("gender")
   lazy val land = zlurp("land")
   lazy val opname = zlurp("opname")

   lazy val persoon = zlurp("persoon")
   lazy val persoon__woonplaats = zlurp("persoon__woonplaats")
   lazy val persoon__beroepsplaats = zlurp("persoon__beroepplaats")
   lazy val plaats = zlurp("plaats")
   lazy val regio = zlurp("regio")
   lazy val relatie = zlurp("relatie")
   lazy val transcriptie = zlurp("transcriptie")
   lazy val transcriptie__bestand = zlurp("transcriptie__bestand")
   lazy val opname__persoon = zlurp("opname__persoon")




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