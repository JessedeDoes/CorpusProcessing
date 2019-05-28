package posmapping

import database.{Configuration, Database}
import database.DatabaseUtilities.Select

import scala.xml.Text

object VMNWdb {
  object vmnwconfig extends Configuration("vmnw", "svowdb02", "VMNW", "impact", "impact", driver="mysql")

  val mainPoS = List("vnw.bw.", "bw.", "bnw.", "lidw.", "ww.", "znw.", "vnw.", "vw.", "vz.", "az.", "telw.")

  case class QuotationReference(woordvorm_nr: Int, citaat_id: Int, ldb_lemma_nr: Int)


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

    /*
    +----------------+
| woordsoort_afk |
+----------------+
| aanw.vnw.      |
| aanw.vnw.bw.   |
| betr.vnw.      |
| betr.vnw.bw.   |
| bez.vnw.       |
| onbep.vnw.     |
| onbep.vnw.bw.  |
| pers.vnw.      |
| vnw.           |
| vnw.bw.        |
| vr.vnw.        |
| vr.vnw.bw.     |
| wdk.vnw.       |
| wkg.vnw.       |
+----------------+

+----------------+
| woordsoort_afk |
+----------------+
| aanw.vnw.bw.   |
| betr.vnw.bw.   |
| onbep.vnw.bw.  |
| vnw.bw.        |
| vr.vnw.bw.     |
+----------------+

     */
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

    lazy val supportedByAttestation = citaatLinks.exists(_.ldb_lemma_nr == this.ldb_lemma_nr)
    lazy val element = <ref type="dictionary.VMNW"
                            citaatId={citaatLinks.headOption.map(x => Text(x.citaat_id.toString))}
                            n={clitisch_deel_nr.toString}
                            target={ldb_lemma_nr.toString}><lemma>{lemmatekst_vmnw}</lemma> <def>{korte_bet}</def> <pos>{woordsoort_afk}</pos></ref>
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

  lazy val citaatLinks = db.iterator(citaatLinkQuery)

  lazy val citaatLinkMap: Map[Int,Set[QuotationReference]] = citaatLinks.toSet.groupBy(_.woordvorm_nr)

  lazy val allemaal = db.iterator(lemmaWoordvormQuery)
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
    val candidates = woordvormLemma.get(woordvorm_nr).getOrElse(Set())

    val filtered0 = candidates.map(_.copy(citaatLinks = citaatLinkMap.getOrElse(woordvorm_nr, Set()))).toSet

    val wouldBeNice:List[LemmaWoordvorm => Boolean] =
      List(_.compatible,
        _.status_lemma_vmnw == 0,
        _.supportedByAttestation,
        _.featureCompatible)

    preferList(filtered0, wouldBeNice)
  }

  def linkXML(wordId: String) =
  {
    val l = findDictionaryLinks(wordId)
    val n = l.size
    if (l.isEmpty) <nolink/> else <xr type="dictionaryLinks" extent={n.toString}>{l.map(_.element).toSeq}</xr>
  }

  def main(args: Array[String]): Unit = {
    allemaal.filter(x => x.mainPos.isEmpty).foreach(x => println(s"$x -->  ${x.mainPos}"))
  }
}