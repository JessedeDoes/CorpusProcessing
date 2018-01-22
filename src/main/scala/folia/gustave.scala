package folia

import scala.xml._

object gustave {
  def readGustave(f: String): Unit =
  {
    val g = XML.load(f)
  }
}

object example
{
  val weetje =   <w xml:id="untitled.p.1.s.1.w.2" class="WORD">
    <t>Aan</t>
    <pos class="VZ(init)" confidence="0.980282" head="VZ">
      <feat class="init" subset="vztype"/>
    </pos>
    <lemma class="aan"/>
    <alt xml:id="untitled.p.1.s.1.w.2.alt.1" src="gustave">
      <lemma class="aan" src="gustave"/>
      <pos class="VZ(init)" confidence="0" head="VZ" src="gustave">
        <feat class="init" subset="vztype"/>
      </pos>
    </alt>
  </w>

  val merge =
    <w xml:id="untitled.p.3.s.1.w.48" class="WORD">
    <t>de</t>
    <pos class="LID(bep,stan,rest)" confidence="0.730679" head="LID">
      <feat class="bep" subset="lwtype"/>
      <feat class="stan" subset="naamval"/>
      <feat class="rest" subset="npagr"/>
    </pos>
    <lemma class="de"/>
    <alt xml:id="untitled.p.3.s.1.w.48.merge.1" src="gustave-merge">
      <merge src="untitled.p.3.s.1.w.48" dest="untitled.p.3.s.1.w.49"/>
      <merge src="untitled.p.3.s.1.w.49" dest="untitled.p.3.s.1.w.49"/>
    </alt>
  </w>
    <w xml:id="untitled.p.3.s.1.w.49" class="WORD">
      <t>zelve</t>
      <pos class="BW()" confidence="0.997988" head="BW"/>
      <lemma class="zelve"/>
      <alt xml:id="untitled.p.3.s.1.w.49.alt.1" src="gustave">
        <lemma class="dezelfde" src="gustave"/>
        <pos class="LID(bep,+forme,+nonnom)" confidence="0" head="LID" src="gustave">
          <feat class="bep" subset="lwtype"/>
          <feat class="+forme" subset="buiging"/>
          <feat class="+nonnom" subset="naamval"/>
        </pos>
      </alt>
      <alt xml:id="untitled.p.3.s.1.w.49.merge.1" src="gustave-merge">
        <merge src="untitled.p.3.s.1.w.48" dest="untitled.p.3.s.1.w.49"/>
        <merge src="untitled.p.3.s.1.w.49" dest="untitled.p.3.s.1.w.49"/>
      </alt>
    </w>
}