package posmapping

import database.{Configuration, Database}
import database.DatabaseUtilities.Select

import scala.collection.immutable
import scala.xml.Text
import scala.util.{Try, Failure, Success}

object Hilex
{
  object hilexconfig extends Configuration("hilex", "svowdb06", "gigant_hilex_candidate", "fannee", "Cric0topus")
  lazy val db = new Database(hilexconfig)
  case class Omspellemma(hist_lemma_id: String, hist_lemma: String, modern_lemma: String)
  {
    lazy val ldb_lemma_nr: Try[Int] = Try(hist_lemma_id.replaceAll("VMNW","").toInt)
  }

  val q = Select( r => Omspellemma(
    r.getString("hist_lemma_id"),
    r.getString("hist_lemma"),
    r.getString("modern_lemma")
  ), "marijke_spelling.spelling where hist_lemma_id ~ 'VMNW'")

  lazy val vmnw_omspellingen = db.slurp(q).filter(_.ldb_lemma_nr.isInstanceOf[Success[Int]]).map(x => x.ldb_lemma_nr.get -> x).toMap

  def main(args: Array[String]): Unit = {
    vmnw_omspellingen.foreach(println)
  }
}

object VMNWdb {

  object vmnwconfig extends Configuration("vmnw", "svowdb02", "VMNW", "impact", "impact", driver="mysql")




  val mainPoS = List("vnw.bw.", "bw.", "bnw.", "lidw.", "ww.", "znw.", "vnw.", "vw.", "vz.", "az.", "telw.")

  case class QuotationReference(woordvorm_nr: Int, citaat_id: Int, ldb_lemma_nr: Int)


  case class Lemma(ldb_lemma_nr: Int, lemmatekst_cg: String, lemmatekst_vmnw: String,
                                             woordsoort_afk: String, status_lemma_vmnw: Int, korte_bet: String)

  lazy val lemmaQuery = Select(r => Lemma(
    r.getInt("ldb_lemma_nr"),

    r.getString("lemmatekst_cg").replaceAll("-[Xx]$","").toLowerCase(),
    r.getString("lemmatekst_vmnw"),
    r.getString("woordsoort_afk"),
    r.getInt("status_lemma_vmnw"),
    r.getString("korte_bet")
  ), "ldb_lemma")


  lazy val allLemmata: Map[Int, Lemma] = db.slurp(lemmaQuery).map(x => x.ldb_lemma_nr -> x).toMap



  case class LemmaWoordvorm(ldb_lemma_nr: Int, woordvorm_nr: Int, lemmatekst_cg: String, lemmatekst_vmnw: String,
                            woordsoort_afk: String, morfkode: Int, clitisch_deel_nr: Int, status_lemma_vmnw: Int, korte_bet: String, citaatLinks: Set[QuotationReference] = Set.empty)
  {
    lazy val mainPos: Option[String] = mainPoS.find(s => woordsoort_afk.contains(s))

    lazy val compatible: Boolean = {
      if (mainPos.isDefined) // ToDo also match features, e.g. relative pronoun etc
      {
        val ws = mainPos.get
        ws match {
          case "vnw.bw." => morfkode >= 600 & morfkode < 700
          case "bw." => morfkode >= 500 & morfkode < 600
          case "znw." => morfkode < 100
          case "ww." => morfkode >= 200 & morfkode < 300
          case "bnw." => morfkode >= 100 & morfkode < 200
          case "telw." => morfkode >= 300 & morfkode < 400
          case "vnw." => morfkode >= 400 & morfkode < 470 | morfkode >= 490 & morfkode < 500
          case "lidw." => morfkode >= 400 & morfkode < 490
          case "vz." => morfkode >= 700 & morfkode < 800
          case "az." => morfkode >= 700 & morfkode < 800 // ?
          case "vw." => morfkode >= 800 & morfkode < 900
          case _ => true
        }
      } else true;
    }

    lazy val featureCompatible = compatible &&
      {
        mainPos match {

          case Some("vnw.") =>
            val klopt = if (woordsoort_afk.contains("aanw")) morfkode >= 410 & morfkode < 420
            else if (woordsoort_afk.contains("betr")) morfkode >= 420 & morfkode < 440
            else if (woordsoort_afk.contains("bez")) morfkode >= 450 & morfkode < 460
            else if (woordsoort_afk.contains("onbep")) morfkode >= 440 & morfkode < 450
            else if (woordsoort_afk.contains("pers")) morfkode >= 401 & morfkode < 407
            else if (woordsoort_afk.contains("vr")) morfkode >= 420 & morfkode < 440
            else if (woordsoort_afk.contains("wdk")) morfkode >= 460 & morfkode < 470
            else if (woordsoort_afk.contains("wkg")) morfkode >= 460 & morfkode < 470
            else true
            klopt
          case Some("vnw.bw.") =>
            val klopt = if (woordsoort_afk.contains("aanw")) morfkode >= 610 & morfkode < 620
            else if (woordsoort_afk.contains("betr")) morfkode >= 620 & morfkode < 640
            //else if (woordsoort_afk.contains("bez")) morfkode >= 450 & morfkode < 460
            else if (woordsoort_afk.contains("onbep")) morfkode >= 640 & morfkode < 650
            else if (woordsoort_afk.contains("vr")) morfkode >= 620 & morfkode < 640
            else true
            klopt
          case Some("bw.") =>
            val klopt = if (woordsoort_afk.contains("aanw")) morfkode >= 510 & morfkode < 520
            else if (woordsoort_afk.contains("betr")) morfkode >= 520 & morfkode < 540
            //else if (woordsoort_afk.contains("bez")) morfkode >= 450 & morfkode < 460
            else if (woordsoort_afk.contains("onbep")) morfkode >= 540 & morfkode < 550
            else if (woordsoort_afk.contains("vr")) morfkode >= 520 & morfkode < 540
            else true
            klopt
          case _ => true
        }
      }

    def patchVerwijzing = {
      val vw = verwijzingen.get(this.ldb_lemma_nr)
      if (vw.isEmpty || this.status_lemma_vmnw != 5) // 5 is verwijslemma ?
        this
      else
        {
           val lem = allLemmata.get(vw.get.target_lemma_nr)
           if (lem.isEmpty)
             this
          else
             {
               val l = lem.get
               Console.err.println(s"Jawel, Patching $this to $l")
               this.copy(ldb_lemma_nr = l.ldb_lemma_nr,
                 lemmatekst_vmnw = l.lemmatekst_vmnw,
                 lemmatekst_cg = l.lemmatekst_cg,
                 korte_bet = l.korte_bet,
                 woordsoort_afk = l.woordsoort_afk,
                 status_lemma_vmnw = l.status_lemma_vmnw)
             }
        }
    }

    lazy val omspelling_uit_hilex: Option[String] = Hilex.vmnw_omspellingen.get(ldb_lemma_nr).map(_.modern_lemma)

    lazy val betere_omspelling = omspelling_uit_hilex.getOrElse(lemmatekst_cg)

    lazy val supportedByAttestation = citaatLinks.exists(_.ldb_lemma_nr == this.ldb_lemma_nr)
    lazy val element = <ref type="dictionary.VMNW"
                            citaatId={citaatLinks.headOption.map(x => Text(x.citaat_id.toString))}
                            n={clitisch_deel_nr.toString}
                            target={ldb_lemma_nr.toString}><modern_lemma>{betere_omspelling}</modern_lemma><lemma>{lemmatekst_vmnw}</lemma> <def>{korte_bet}</def> <pos>{woordsoort_afk}</pos></ref>


  }

  val db = new Database(vmnwconfig)


  lazy val lemmaWoordvormQuery = Select(r => LemmaWoordvorm(
    r.getInt("ldb_lemma_nr"),
    r.getInt("woordvorm_nr"),
    r.getString("lemmatekst_cg").replaceAll("-[Xx]$","").toLowerCase(),
    r.getString("lemmatekst_vmnw"),
    r.getString("woordsoort_afk"),
    r.getInt("morfkode"),
    r.getInt("clitisch_deel_nr"),
    r.getInt("status_lemma_vmnw"),
    r.getString("korte_bet")
  ), "lemma_woordvorm_view_alt")

  lazy val citaatLinkQuery = Select(r => QuotationReference(
    r.getInt("woordvorm_nr"),
    r.getInt("citaat_id"),
    r.getInt("ldb_lemma_nr")
  ),   "citaat_tokens where is_attestatie=true")

  /*
  select verwijzing.*, ldb_lemma.ldb_lemma_nr from verwijzing, ldb_lemma where verwijzing.soort_vw ='zie' and verwijzing.verw_tekst=ldb_lemma.lemmatekst_vmnw and verwijzing.attr_naam='LEMMATEKST_VMNW';

  +--------------------+-------------+------+-----+---------+-------+
| Field              | Type        | Null | Key | Default | Extra |
+--------------------+-------------+------+-----+---------+-------+
| ldb_lemma_nr       | int(11)     | NO   |     | 0       |       |
| attr_naam          | varchar(25) | NO   |     |         |       |
| identificatie_attr | varchar(25) | NO   |     |         |       |
| soort_vw           | varchar(10) | YES  |     | NULL    |       |
| verw_tekst         | mediumtext  | YES  |     | NULL    |       |
| verw_volgnr        | smallint(6) | NO   |     | 0       |       |
| target_lemma_nr    | int(11)     | NO   |     | 0       |       |
+--------------------+-------------+------+-----+---------+-------+

   */

  case class Verwijzing(
                       ldb_lemma_nr: Int,
                       verw_tekst: String,
                       target_lemma_nr: Int,
                       )

  lazy val verwijsLemmaQuery = Select(
    r => Verwijzing(r.getInt("ldb_lemma_nr"), r.getString("verw_tekst"), r.getInt("target_lemma_nr")),
    "lemma_verwijzingen"
  )

  lazy val verwijzingen: Map[Int, Verwijzing] = db.slurp(verwijsLemmaQuery).map(x => x.ldb_lemma_nr -> x).toMap

  lazy val citaatLinks = db.iterator(citaatLinkQuery)

  lazy val citaatLinkMap: Map[Int,Set[QuotationReference]] = citaatLinks.toSet.groupBy(_.woordvorm_nr)

  lazy val allemaal = db.iterator(lemmaWoordvormQuery).map(_.patchVerwijzing)

  lazy val woordvormLemma = {
    val z = allemaal.toList.groupBy(_.woordvorm_nr)
    Console.err.println("Yoho links collected!!!")
    z
  }

  def prefer[T](s: Set[T], f: T => Boolean) = if (s.exists(f)) s.filter(f) else s

  def preferList[T](s0: Set[T], fs: List[T => Boolean]) = fs.foldLeft(s0)({case (s,f) => prefer[T](s,f)})

  def findDictionaryLinks(wordId: String) =
  {
    val woordvorm_nr = wordId.replaceAll("[^0-9]", "").toInt
    val candidates: immutable.Iterable[LemmaWoordvorm] with (Nothing => Any) = woordvormLemma.get(woordvorm_nr).getOrElse(Set())

    val filtered0 = candidates.map(_.copy(citaatLinks = citaatLinkMap.getOrElse(woordvorm_nr, Set()))).toSet

    val wouldBeNice:List[LemmaWoordvorm => Boolean] =
      List(_.compatible,
        _.status_lemma_vmnw == 0,
        _.supportedByAttestation,
        _.featureCompatible)

    preferList(filtered0, wouldBeNice)
  }

  def linkXML(wordId: String) = // dit is niet goed, moet je per clitisch deeltje doen
  {
    val l: Set[LemmaWoordvorm] = findDictionaryLinks(wordId)
    val mogelijke_marijkes = l.map(_.omspelling_uit_hilex).filter(_.nonEmpty).map(_.get).toSet
    //System.err.println("Hilex moderne lemmavormen:"  + mogelijke_marijkes)
    val n = l.size

    val elem = if (l.isEmpty) <nolink/> else <xr type="dictionaryLinks" extent={n.toString}>{l.map(_.element).toSeq}</xr>
    (elem, l)
  }


  def main(args: Array[String]): Unit = {
    allemaal.filter(x => x.mainPos.isEmpty).foreach(x => println(s"$x -->  ${x.mainPos}"))
  }
}