package posmapping

import propositional.Proposition

import scala.xml.{Elem, XML}

case class Feature(name: String, value: String)
{
  override def toString: String = s"$name=$value"
}

abstract class Tag(tag: String, tagset: TagSet)
{
  val pos:String
  val features:List[Feature]
  def parseFeature(f:String):Feature
  def proposition:Proposition
}


trait TagParser
{
  def parseTag(ts: TagSet, t: String): Tag
}

object  defaultTagParser extends TagParser
{
  override def parseTag(ts: TagSet, t: String): Tag = new CGNStyleTag(t, ts)
}


object MolexTagSet extends TagSet("molex")
{
  override def fromProposition(p:Proposition, posIsSpecial: Boolean = false):Tag = UDTagSet.fromProposition(p)
  override def fromString(t: String): Tag = new UDStyleTag(t, this)
}

object TagSet
{
  def fromXML(d: Elem):TagSet =
  {
    val prefix = (d \ "prefix").text
    val posTags = (d \\ "pos").map(_.text.trim).toSet.toList
    val partitions = (d \\ "partitions" \ "feature").map(f => (f \ "name").text -> (f \\ "value").map(_.text).toSet ).toMap
    val pos2partitions = (d \\ "constraints" \ "constraint").map(f => (f \ "pos").text -> (f \\ "feature").map(_.text).toList ).toMap
    TagSet(prefix, posTags, partitions, pos2partitions)
  }

  def fromXML(f: String):TagSet = fromXML(XML.load(f))
}

case class TagSet(prefix: String,
                  val posTags: List[String] = List.empty,
                  partitions:Map[String,Set[String]] = Map.empty,
                  pos2partitions: Map[String,List[String]] = Map.empty, parser: TagParser=defaultTagParser)
{

  def toXML =
    <tagset>
      <prefix>{prefix}</prefix>
      <mainPoS>
        {posTags.map(p => <pos>{p}</pos>)}
      </mainPoS>
      <partitions>
        {partitions.map( { case (f, vs) => <feature><name>{f}</name><values>{vs.map(v => <value>{v}</value>)}</values></feature>} )}
      </partitions>
      <constraints>
        {pos2partitions.map( { case (p, fs) => <constraint><pos>{p}</pos><features>{fs.map(f => <feature>{f}</feature>)}</features></constraint>} )}
      </constraints>
    </tagset>

  def asPlainText =
    s"""
       |[features]
       |${partitions.map( {case (f,vs) => s"$f=${vs.mkString(", ")}" } ).mkString("\n")}
       |
       |[constraints]
       |${pos2partitions.map( {case (p,fs) => s"$p=${fs.mkString(", ")}" } ).mkString("\n")}
     """.stripMargin

  def inSubsets(f:String):List[String] = partitions.filter({ case (s, v) => v.contains(f) }).toList.map(_._1)

  def fromPropositionCGN(p:Proposition, posIsSpecial: Boolean = false):Tag =
  {
    val vars = p.varsIn

    val pos = vars.find(v => v.startsWith(s"${this.prefix}:pos=")).getOrElse("UNK").replaceAll(".*=", "")
    val features = vars.filter(f => !f.startsWith(s"${this.prefix}:pos=")).toList
      .sortBy(f => {
        val n = f.replaceAll(s"^$prefix:feat.|=.*","")
        //Console.err.println(v)
        val l = pos2partitions(pos)
        val i = l.zipWithIndex.find({case (s,i) => s == n}).map({case (s,i) => i})
        i.getOrElse(0)
      })
      .map(v => v.replaceAll(s"^.*=", "")).mkString(",")

    var x = new CGNStyleTag(s"$pos($features)", this)
    //Console.err.println(s"Proposition to tag: $p->$x")
    x
  }

  def fromProposition(p:Proposition, posIsSpecial: Boolean = false):Tag = fromPropositionCGN(p,posIsSpecial)


  def featuresFromProposition(p:Proposition): Set[Feature] =
  {
    val vars = p.varsIn
    val features = vars.map(v => v.replaceAll(s"^${this.prefix}:", "")).map(x => {val r = x.split("="); Feature(r(0), r(1))})
    features
  }

  def fromString(t: String):Tag = parser.parseTag(this,t)

  def asTEIFeatureStructure(s: String):Elem = asTEIFeatureStructure(fromString(s))

  def asTEIFeatureStructure(t:Tag):Elem = <fs type={prefix}>
    {t.features.map(f => <f name={f.name}><symbol value={f.value}/></f>)}
  </fs>
}
