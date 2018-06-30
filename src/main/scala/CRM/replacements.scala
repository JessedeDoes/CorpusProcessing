package CRM

object ents {

  val entities:Map[String, String] = List(
    ("&komma;", ","),
    ("&excl;", "!"),
    ("&2periods;", ".."),

    ("u&uml;", "ü"),
    ("ouml;", "ö"),
    ("a&uml;", "ä"),
    ("y&uml;", "ÿ"),
    ("e&uml;", "ë"),
    ("v&uml;", "v̈"),
    ("&duitsekomma;", "/"),

    ("&super;", ""), // ahem nog iets mee doen, weet niet precies wat

    ("o&grave;", "ò"),
    ("&period;", "."),
    ("&semi;", ";"),
    ("&tilde;", "~"),

    //("&scheider;", 17664)
    ("&r;", "°"), // soms samen met vorige karakter, zie crm.xml
    ("&hyph;", "-"),
    ("&unreadable;", "?"),
    ("&colon;", ":"),
    ("&quest;", "?")
  ).toMap;

  import java.text.Normalizer
  def noAccents(s: String):String = Normalizer.normalize(s, Normalizer.Form.NFD).replaceAll("\\p{M}", "").toLowerCase.trim


  val entityPattern = entities.keySet.mkString("|").r
  def replaceEnts(s: String):String = replacements.replacements(s) // entityPattern.replaceAllIn(s, m => entities(m.group(0))) // LASTIG MET ALIGNMENTS!!!
}

object replacements {
  val filters =     <filters>
    <!--
    <filter type="replace" name="4" value="*" replace="" />
    <filter type="replace" name="4" value="#" replace="" />
    -->
    <filter type="replace" name="0,1,2,3" value="&amp;excl;" replace="!" />
    <filter type="replace" name="0,1,2,3" value="&amp;duitsekomma;" replace="/" />
    <filter type="replace" name="0,1,2,3" value="&amp;period;" replace="." />
    <filter type="replace" name="0,1,2,3" value="&amp;komma;" replace="," />
    <filter type="replace" name="0,1,2,3" value="&amp;unreadable;" replace="[unreadable]" />
    <filter type="replace" name="0,1,2,3" value="&amp;hyph;" replace="-" />
    <filter type="replace" name="0,1,2,3" value="&amp;semi;" replace=";" />
    <filter type="replace" name="0,1,2,3" value="&amp;colon;" replace=":" />
    <filter type="replace" name="0,1,2,3" value="&amp;2periods;" replace=".." />
    <filter type="replace" name="0,1,2,3" value="&amp;tilde;" replace="~" />
    <filter type="replace" name="0,1,2,3" value="&amp;quest;" replace="?" />
    <filter type="replace" name="0,1,2,3" value="u&amp;r;" replace="ů" />
    <filter type="replace" name="0,1,2,3" value="U&amp;r;" replace="Ů" />
    <filter type="replace" name="0,1,2,3" value="o&amp;r;" replace="o̊" />
    <filter type="replace" name="0,1,2,3" value="O&amp;r;" replace="O̊" />
    <filter type="replace" name="0,1,2,3" value="a&amp;r;" replace="å" />
    <filter type="replace" name="0,1,2,3" value="A&amp;r;" replace="Å" />
    <filter type="replace" name="0,1,2,3" value="d&amp;r;" replace="d̊̊" />
    <filter type="replace" name="0,1,2,3" value="D&amp;r;" replace="D̊" />
    <filter type="replace" name="0,1,2,3" value="e&amp;r;" replace="e̊" />
    <filter type="replace" name="0,1,2,3" value="E&amp;r;" replace="E̊" />
    <filter type="replace" name="0,1,2,3" value="g&amp;r;" replace="g̊̊" />
    <filter type="replace" name="0,1,2,3" value="G&amp;r;" replace="G̊" />
    <filter type="replace" name="0,1,2,3" value="y&amp;r;" replace="ẙ" />
    <filter type="replace" name="0,1,2,3" value="Y&amp;r;" replace="Y̊" />
    <filter type="replace" name="0,1,2,3" value="s&amp;r;" replace="s̊" />
    <filter type="replace" name="0,1,2,3" value="S&amp;r;" replace="S̊̊" />
    <filter type="replace" name="0,1,2,3" value="w&amp;r;" replace="ẘ" />
    <filter type="replace" name="0,1,2,3" value="W&amp;r;" replace="W̊" />
    <filter type="replace" name="0,1,2,3" value="h&amp;r;" replace="h°" />
    <filter type="replace" name="0,1,2,3" value="H&amp;r;" replace="H°" />
    <filter type="replace" name="0,1,2,3" value="t&amp;r;" replace="t°" />
    <filter type="replace" name="0,1,2,3" value="T&amp;r;" replace="T°" />
    <filter type="replace" name="0,1,2,3" value="r&amp;r;" replace="r°" />
    <filter type="replace" name="0,1,2,3" value="R&amp;r;" replace="R°" />
    <filter type="replace" name="0,1,2,3" value="m&amp;r;" replace="m°" />
    <filter type="replace" name="0,1,2,3" value="M&amp;r;" replace="M°" />
    <filter type="replace" name="0,1,2,3" value="x&amp;r;" replace="x°" />
    <filter type="replace" name="0,1,2,3" value="X&amp;r;" replace="X°" />
    <filter type="replace" name="0,1,2,3" value="l&amp;r;" replace="l°" />
    <filter type="replace" name="0,1,2,3" value="L&amp;r;" replace="L°" />
    <filter type="replace" name="0,1,2,3" value="c&amp;r;" replace="c°" />
    <filter type="replace" name="0,1,2,3" value="C&amp;r;" replace="C°" />
    <filter type="replace" name="0,1,2,3" value="v&amp;r;" replace="v°" />
    <filter type="replace" name="0,1,2,3" value="V&amp;r;" replace="V°" />
    <filter type="replace" name="0,1,2,3" value="i&amp;r;" replace="i°" />
    <filter type="replace" name="0,1,2,3" value="I&amp;r;" replace="I°" />
    <filter type="replace" name="0,1,2,3" value="j&amp;r;" replace="j°" />
    <filter type="replace" name="0,1,2,3" value="J&amp;r;" replace="J°" />
    <filter type="replace" name="0,1,2,3" value="n&amp;r;" replace="n°" />
    <filter type="replace" name="0,1,2,3" value="N&amp;r;" replace="N°" />
    <filter type="replace" name="0,1,2,3" value="a&amp;uml;" replace="ä" />
    <filter type="replace" name="0,1,2,3" value="A&amp;uml;" replace="Ä" />
    <filter type="replace" name="0,1,2,3" value="e&amp;uml;" replace="ë" />
    <filter type="replace" name="0,1,2,3" value="E&amp;uml;" replace="Ë" />
    <filter type="replace" name="0,1,2,3" value="y&amp;uml;" replace="ÿ" />
    <filter type="replace" name="0,1,2,3" value="Y&amp;uml;" replace="Ÿ" />
    <filter type="replace" name="0,1,2,3" value="u&amp;uml;" replace="ü" />
    <filter type="replace" name="0,1,2,3" value="U&amp;uml;" replace="Ü" />̈
    <filter type="replace" name="0,1,2,3" value="v&amp;uml;" replace="v̈" />
    <filter type="replace" name="0,1,2,3" value="V&amp;uml;" replace="V̈" />
    <filter type="replace" name="0,1,2,3" value="o&amp;uml;" replace="ö" />
    <filter type="replace" name="0,1,2,3" value="O&amp;uml;" replace="Ö" />
    <filter type="replace" name="0,1,2,3" value="o&amp;grave;" replace="ò" />
    <filter type="replace" name="0,1,2,3" value="O&amp;grave;" replace="Ò" />
    <filter type="replace" name="0,1,2,3" value="e&amp;super;" replace="&#x034F;&#x0364;" />
    <filter type="replace" name="0,1,2,3" value="v&amp;super;" replace="&#x034F;&#x036E;" />
    <filter type="replace" name="0,1,2,3" value="r&amp;super;" replace="&#x034F;&#x036C;" />
    <filter type="replace" name="0,1,2,3" value="i&amp;super;" replace="&#x034F;&#x0365;" />
    <filter type="replace" name="0,1,2,3" value="o&amp;super;" replace="&#x034F;&#x0366;" />
    <filter type="replace" name="0,1,2,3" value="&amp;r;" replace="&#x034F;&#x030A;" />
  </filters>

  val allReplacements:Seq[String => String] = (filters \ "filter").map( f =>
  {
    val p = (f \ "@value").text.replaceAll("&amp;", "&")
    val r = (f \ "@replace").text
    s:String => s.replaceAll(p,r)
  })

  def replacements(s: String) = allReplacements.foldLeft(s)( (s,f) => f(s))
}
