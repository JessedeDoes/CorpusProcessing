package Wp6

import scala.xml._
import scala.util.matching.Regex._

case class NodeWithOffsets(node: Node, start: Int, end: Int, children: Seq[NodeWithOffsets])
{
  def label = node.label
  def \(s: String): Seq[NodeWithOffsets] = children.filter(_.node.label == s)
  def \\(s: String): Seq[NodeWithOffsets] = \(s) ++ children.flatMap(x => x.\\(s))
  def text = node.text
}

object StandoffMarkup {

  case class Token(word: String, start:Int, end: Int)  {

  }

  val wordPattern = "\\S+".r

  def extend(tokens: Seq[Token], lastTokenClosed: Boolean, m: Match, startOffset: Int, textLength: Int): (Seq[Token], Boolean) = {

    // Console.err.println(s"$m ${m.end} $textLength")

    if (tokens.isEmpty || lastTokenClosed || m.start > 0)
      {
        val newT = new Token(m.toString, startOffset + m.start, startOffset +  m.end)
        (tokens :+ newT) -> (m.end < textLength)
      } else {
      val tLast = tokens.last
      val tNewLast = tLast.copy(word=tLast.word + m.toString, end=startOffset + m.end)
      (tokens.dropRight(1) :+ tNewLast) -> (m.end < textLength)
    }
  }

  def extendWithNode(tokens: Seq[Token], lastTokenClosed: Boolean, n: NodeWithOffsets, isTokenBoundary: Node => Boolean = z => true): (Seq[Token], Boolean)  = {
    n.node match {
      case Text(t) =>
        val words = wordPattern.findAllMatchIn(t).toList
        if (words.isEmpty)
          (tokens, t.nonEmpty)
        else
        {
          words.foldLeft(tokens -> lastTokenClosed){case ((t1, b),m) => extend(t1,b,m, n.start, t.length)}
        }
      case e: Elem =>
        // if (e.label == "hi") Console.err.println(e)
        val b0 = lastTokenClosed || isTokenBoundary(e)
        // if (e.label == "hi") Console.err.println(b0)
        val (foldedToks, closed) =  n.children.foldLeft(tokens -> b0){case ((t, b), n1) => extendWithNode(t,b,n1,isTokenBoundary)}
        // if (e.label == "hi") Console.err.println(s"${ (foldedToks, closed)}")
        foldedToks -> (closed || isTokenBoundary(e))
    }
  }

  def tokenize(n: NodeWithOffsets, isTokenBoundary: Node => Boolean = z => true) = {
    extendWithNode(Seq(), true, n, isTokenBoundary)
  }

  def createStandoffMarkup(e: Node, startPosition: Int=0): NodeWithOffsets = {
    val initial = (Seq[NodeWithOffsets](), startPosition)

    def gadoor(seq: Seq[NodeWithOffsets], p: Int, n: Node) = {
      val n1 = createStandoffMarkup(n, p)
      (seq ++ Seq(n1), n1.end)
    }

    val (children, z): (Seq[NodeWithOffsets], Int) = e.child.foldLeft(initial) { case ((seq, p), n) => gadoor(seq, p, n) }
    val last = if (children.nonEmpty) children.last.end else startPosition + e.text.length
    NodeWithOffsets(e, startPosition, last, children)
  }

  def main(args: Array[String]): Unit = {

    val x = createStandoffMarkup(example)
    val x1 = createStandoffMarkup(XML.loadFile("/tmp/match.INT_d9e3fee8-0b2e-36d9-910b-9681e4d60ef4.xml"))

    val inlineTags = Set("lb", "br", "milestone", "hi", "pb")

    val (s,b) = tokenize(x1, n => !(inlineTags.contains(n.label)))

    val text = x1.text
    s.foreach(t => {
      val check = text.substring(t.start, t.end)
      println(s"$t <$check>")
    })
    val names: Seq[NodeWithOffsets] = x1 \\ "name"
    if (false) names.foreach(n => {
      val check = n.node.text
      val content = text.substring(n.start, n.end)
      Console.err.println(s"$content####$check###${n.start}#${n.end}")
    })
    //println(x)
  }

  val example = <text>Van  Outhoorn,  Van  Hoorn,  Pijl,  De  Haas,  Van
  Riebeeck,  enz.  XXIII,  23  nove<lb/>mber  1699  93  is  het
    geselen  en  brandmerken  niet  toe<hi>gepast;  d</hi>e  tweede
    verkocht  zijn  buit  aan  een  Engels  scheepje;  de  water
    ^  fiskaal  Claus<name type="person" norm="Alebos, Claas"
  distance="0">Alebos</name>   was  nalatig  ter  zake  ,
  zodat  de  vervolging  aan  den  provisionelen  fiskaal
  <name type="person" norm="Quevellerius, Abraham"
        distance="0">Quevellerius</name>   werd  opgedragen  ,
  <name type="person" norm="Alebos, Claas"
        distance="0">Alebos</name>   werd  daarna  buiten  gage  en
    dienst  gestéld;  bij  vonnis  zijn  Willem  <name
  type="person" norm="Valentijn, Willem"
  distance="0">Valentijn</name>   en  Nicolaas  <name
  type="person" norm="Spring(h), Nicolaas"
  distance="0">Spring</name>   wegens  malversatiën  beboet
  en  buiten  ambt  en  gage  gesteld;  de  repatriërende
    <name type="person" norm="Valckenier, Mr. Wouter"
          distance="0">Valckenier</name>   zal  rapporteren  over  de
  constitutie  van  de  Raad  van  Justitie,  waarvan  hij
    president  was  ,  uit  patria  kwam  als  lid  mr.  Comelis
  <name type="person" norm="Berendregt, Mr. Comelis (van)"
        distance="0">Berendregt,</name>   maar  27  oct.  stierf
  het  lid  Philippus  <name type="person" norm="Muykens,
Philippus" distance="0">Muykens;</name>   ds.  <name
  type="person" norm="Kiesinga, Johannes"
  distance="1">Kiesenga,</name>   in  conflict  met  de
    kerkeraad,  had  ,  zijn  gage  behoudend,  moeten
  repatriëren,  maar  mag  op  zijn  verzoek  nog  een  jaar
    overblijven,  hij  deed  voorts  een  poging  tot
    reconciliatie,  de  aangelegenheid  betreft  slechts  de
  heftige  tegenspraak  tegen  de  aan  een  lidmaat
  opgelegde  censuur;  ds.  <name type="person"
  norm="Hupperts, Gosuinus" distance="0">Hupperts</name>
  werd  te  <name type="location" norm="Cochin"
                  distance="0">Cochin</name>   beroepen,  de  mede  pas
  aangekomen  predikanten  Boter  koper  en  <name
  type="person" norm="Bartoe, Johannes (Joan)"
  distance="0">Bartoe</name>   zijn  nog  te  <name
  type="location" norm="Batavia" distance="0">Batavia;</name>
  sedert  10  febr.  kwamen  13  Engelse  schepen  te  <name
  type="location" norm="Batavia" distance="0">Batavia;</name>
  plakaat  tegen  de  handel  met  vreemde  Europeanen,  vnl.
    de  Eng  éisen,  Plakaatboek  III,  p.  469,  4  sept.  -
    14  oct.  1699).  Hoewel  sommige  van  die  vrienden
  hebben  geoordeelt  ’tselve  teenemaal  te  strijden  tegen
  de  nauwe  alliantie,  die  der  tegenwoordig  tusschen  de
  twee  natiën  werd  onderhouden  en  uyt  dien  hoofde  haar
  niet  ongeoorloft  te  weesen  jaar  uyt  jaar  in  hier  te
    verblijven  en  domicilium  te  hebben  tot  geen  anderen
    eynde  als  om  voor  factoor  of  makelaar  van  haar
  aankomende  en  vertrekkende  makkers  te  ageren,  daar  se
    om  gewin  te  doen  eenige  onser  ingeseten  wel  in
  weten  te  trekken,  als  uyt  de  documenten,  ten  laste
    van  <name type="person" norm="Godschalk, Pieter"
  distance="0">Godschalk</name>   belegt  en  nu  nogmaal
    overgaande,  kan  werden  vernomen.  (  Een  van  hen  wilde
    zélfs  ondanks  de  last  van  den  Gouverneur  -Generaal
    niet  vertrekken,  waarom  hij  bij  resolutie  moest
    worden  weggezonden,  men  behandelt  de  Engélsen  echter
    beleefd,  gaf  zelfs  scheepstransport  aan  enigen,  die
    in  geschil  waren  met  hun  chefs;
    <name type="person" norm="Valckenier, Mr. Wouter" distance="0">Valckenier</name>
  als  admiraal  en  <name type="person" norm="Lycochthon,
Wijbrand" distance="1">Lycochton</name>   als  vice-admiraal
  vertrekken  naar  patria;  21617  ffi  kruidnagelstof  is
    in  een  modderpoel  weggeworpen,  maar  vermijterde
    brokken  notemuscaat  zullen  nog  naar  patria  worden
  gezonden,  misschien  met  de  peperschepen,  die  Heren
    XVII  geprojecteerd  hebben;  de  <name type="ship"
  norm="Faam" distance="0">Faam,</name>   Kasteel  <name
  type="location" norm="Batavia" distance="0">Batavia,</name>
  <name type="ship" norm="Wognum" distance="2">Wog7ium,</name>
    <name type="ship" norm="Pampus" distance="0">Pampus</name>
  en  <name type="ship" norm="Salland"
  distance="0">Salland</name>   zijn  niet  geschikt  voor
    retourschepen,  waardoor  men  moeite  heeft  de
  retourvloot  voor  de  2e  bezending  bijeen  te  brengen,
  meer  goede  schepen  zijn  nodig).  Door  den  bekende
  gewesen  capitain  Jacob  Jansz.  de  <name type="person"
  norm="Roy, Jacob Jansz. de" distance="0">Roy</name>   sijn
  wij  niet  weynig  om  ’t  een  en  ’t  ander
    vermoeyelijk[t]  geworden  volgens  diverse  ingeleverde
  requesten  sedert  sijn  aankomst  alhier  uyt  <name
  type="location" norm="Bengalen" distance="1">Bengale,</name>
  soo  verre  dat  wij  eyndelijk  bij  resolutie  van  18e
    aug.°  (om  redenen  U  Ed.le  Ho.  Agtb.  wel  bekent)
  geresolveert  sijn  geworden  hem  sijn  vorige  qualiteyt
  en  gagie  van  80  gis.  ter  maand  weer  toe  te  voegen
    en  daarboven  bij  resolutie  van  primo  september  het
    bedragen  van  300  rds.  te  schenken  en  daarmede  na
  Jaffenapatnam  om  daar  in  guarnisoen  te  leggen  te
    versenden.  (Voor  316698  rsd.  werd  in  kassa  ontvangen
    voor  wissels  op  patria,  voor  96700  werd
  overgeschreven  van  gelden,  reeds  in  de  V.O.C.  kas
  berustend,  hieronder  zijn  14000  van  den  visitateur
    -generaal  Thomas  van  <name type="person" norm="Son,
Thomas van" distance="0">Son,</name>   23000  van  de
  weduwe  van  Mattheus  <name type="person" norm="Abouts,
Mattheus" distance="0">Abouts,</name>   33000  van  <name
  type="person" norm="Lycochthon, Wijbrand"
  distance="0">Lycochthon,</name>   23000  van  den
    boekhouder  Nicolaas  de</text>
}
