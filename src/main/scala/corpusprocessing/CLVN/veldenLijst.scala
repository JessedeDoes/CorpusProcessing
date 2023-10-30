package corpusprocessing.CLVN


object veldenLijst {
val anatesAlieni="collectie record_id record_id_level1 record_id_level2 sourceref_level1 sourceref_level2 id_text_part filenaam parent_id tellertje old_idno note".split("\\s+").toSet

val veldNamen = s"""
titleLevel0
titleLevel1
titleLevel2
titleLevel3

subtitleLevel1
subtitleLevel2
subtitleLevel3
subtitleLevel0

authorLevel0
authorLevel1
authorLevel2		

editorLevel1
editorLevel2

editionLevel1
editionLevel2

biblScope_pageLevel1
biblScope_pageLevel2

genreLevel1
genreLevel2

subgenreLevel1
subgenreLevel2

witnessDayLevel1_from
witnessDayLevel1_to

witnessMonthLevel1_from
witnessMonthLevel1_to

witnessYearLevel1_from
witnessYearLevel1_to

witnessDayLevel2_from
witnessDayLevel2_to

witnessMonthLevel2_from
witnessMonthLevel2_to

witnessYearLevel2_from
witnessYearLevel2_to

textDayLevel1_from
textDayLevel1_to

textMonthLevel1_from
textMonthLevel1_to

textYearLevel1_from
textYearLevel1_to 

textDayLevel2_from
textDayLevel2_to

textMonthLevel2_from
textMonthLevel2_to

textYearLevel2_from
textYearLevel2_to


localization_placeLevel1
localization_provinceLevel1
localization_regionLevel1
localization_countryLevel1
localization_kloekecodeLevel1
localization_placeLevel2
localization_provinceLevel2
localization_regionLevel2
localization_countryLevel2
localization_kloekecodeLevel2

textLocalization_placeLevel1
textLocalization_provinceLevel1

categoryLevel1
categoryLevel2

resourceURILevel1
resourceURILevel2

publisherLevel1
publisherLevel2

pubYearLevel1_to
pubYearLevel1_from
pubYearLevel2_to
pubYearLevel2_from

placePublicationLevel1
placePublicationLevel2

primaryLanguageLevel1
primaryLanguageLevel2

copyrightHolder
copyrightOwner

notesStmt
sourceID

factualityLevel1
factualityLevel2
statusTextLevel1
statusTextLevel2

mediumLevel1
mediumLevel2

corpusProvenance

ipr

processingMethod

ingestTime

idno

genreLevel0
                   localization_countryLevel0
                   localization_kloekecodeLevel0
                   localization_placeLevel0
                   localization_provinceLevel0
                   localization_regionLevel0
                   subgenreLevel0
                   textDayLevel0_from
                   textDayLevel0_to
                   textMonthLevel0_from
                   textMonthLevel0_to
                   textYearLevel0_from
                   textYearLevel0_to
                   titleLevel0
                   witnessDayLevel0_from
                   witnessDayLevel0_to
                   witnessMonthLevel0_from
                   witnessMonthLevel0_to
                   witnessYearLevel0_from
                   witnessYearLevel0_to

  """.trim.split("\\s+").toSet

  val laterToeTeVoegen = s"""
categoryLevel1
categoryLevel2
editionLevel1
editionLevel2
mediumLevel1
mediumLevel2
resourceURILevel1
resourceURILevel2
placePublicationLevel1
placePublicationLevel2
publisherLevel1
publisherLevel2
pubYearLevel1_to
pubYearLevel1_from
pubYearLevel2_to
pubYearLevel2_from
factualityLevel1
factualityLevel2
statusTextLevel1
statusTextLevel2
primaryLanguageLevel1
primaryLanguageLevel2
sourceLanguage
copyrightHolder
copyrightOwner
corpusProvenance
processingMethod
titleLevel3
subtitleLevel3
""".trim.split("\\n").toSet

  lazy val allFields = veldNamen ++ anatesAlieni
  lazy val allFieldsLC = allFields.map(_.toLowerCase)

  def fieldOK(s: String) = allFieldsLC.contains(s.toLowerCase())

  def consistency(l1: Int, l2:Int):Boolean = {
    val s2 = allFields.filter(_.endsWith("evel" + l2))
    val s12 = allFields.filter(_.endsWith("evel" + l1)).map(_.replaceAll(l1 + "$", s"${l2}"))
    val s2_s12 = s2 diff s12
    val s12_s2 = s12 diff s2

    println(s"De volgende velden bestaan op level $l2, maar niet op level $l1: $s2_s12")
    println(s"Als je van $l1 naar $l2 gaat, zou je verwachten dat bestaat: $s12_s2, maar ze zijn er niet!")
    s12 == s2
  }

  def consistent = consistency(1,2) && consistency(2,1)

  lazy val lc2camel = allFields.map(f => f.toLowerCase() -> f).toMap ++
    Map("processingmethod_level1" -> "processingMethodLevel1", "processingmethod_level2"  -> "processingMethodLevel2")

  def camelize(f:String): String = if (lc2camel.contains(f)) lc2camel(f) else f

  def main(args: Array[String]) = println(consistent)
}

import utils.{PostProcessXML, ProcessFolder}

import scala.xml._

object allFields
{

  def doFile(f: java.io.File):List[String] = (XML.loadFile(f) \\ "interpGrp").map(i => (i \ "@type").toString()).toList
  def main(args: Array[String]): Unit = {
    def it(s: String) =s
    val allFields = ProcessFolder.processFolder( new java.io.File(args(0)), doFile).flatten.toSet.toList.sortBy(it)
    allFields.foreach(println)
  }
}


object addWordCountToBibls
{
  def blankText(n : Node): String =
  {
    n match {
      case e: Elem =>
        e.child.filter(x => (x \ "@ana").isEmpty).map(blankText).mkString
      case t: Text => t.text
      case _ => ""
    }
  }

  def wordCount(s: String):Int = s.trim.split("\\s+").size

  def wordCount(d: Node): Int = wordCount(blankText(d))

  def anaCounts(d: Elem) =
  {
    d.descendant.filter(n => (n \ "@ana").nonEmpty).map(n =>
      (n \ "@ana").text.replaceAll("#","") -> wordCount(n))
      .groupBy(_._1)
      .mapValues(_.map(_._2))
      .mapValues(_.sum)
  }

  def addCounts(d: Elem, d1: Elem):Elem =
  {
    val countz:Map[String,Int] = anaCounts(d1)
    val justD:Int = blankText((d1 \\ "text").head ).split("\\s+").size

    def updateBibl(b: Elem) =
    {
      val id = (b.attributes.find(_.key == "id")).map(_.value.text)
      val count = if (id.isDefined && countz.contains(id.get))
        countz(id.get)
      else justD
      b.copy(child = <interpGrp type="wordCount"><interp>{count}</interp></interpGrp> ++ b.child)
    }

    PostProcessXML.updateElement(d, _.label == "bibl", updateBibl)
  }

  def fixFile(in: String, out: String) =
  {
    //val m = new tei2folia.Milestone2Seg(in, "/tmp/aap.xml")
    ///m.process()
    throw new Exception("tei2folia is not at home, could not do this...")
    val d1 = XML.load("/tmp/aap.xml")
    val d = addCounts(XML.load(in), d1)
    XML.save(out, d, "UTF-8")
  }


  def main(args: Array[String]): Unit = {
    def it(s: String) =s
    // val allFields = ProcessFolder.processFolder( new java.io.File(args(0)), doFile).flatten
    ProcessFolder.processFolder(new java.io.File(args(0)), new java.io.File(args(1)), fixFile )
    //allFields.foreach(println)
  }
}

object levelCheck
{

  def fieldsFromBibl(b: Node):Map[String,String] = {
    (b \\ "interpGrp").map(b => (b \ "@type").text -> (b \ "interp").text).toMap
  }

  def doFile(f: java.io.File): (java.io.File, Stream[Map[String,String]]) = f ->
    {
      val d = XML.loadFile(f)
      (d \\ "bibl").toStream.map(fieldsFromBibl)
    }

  def main(args: Array[String]): Unit = {
    val veldjes = ProcessFolder.processFolder( new java.io.File(args(0)), doFile)

    val level0Items = veldjes.map({case (f,ps) => ps.tail.size}).sum

    println("Level 0 totaal:" + level0Items)

    val level1Files = veldjes.filter( {case (f,ps) => ps(0).contains("titleLevel1")})

    val level2Files = veldjes.filter( {case (f,ps) => ps(0).contains("titleLevel2") && !(ps(0).contains("titleLevel1"))})

    println("Level 1 totaal:" + level1Files.size)
    println("Level 2 totaal:" + level2Files.size)

    level2Files.foreach(x => println(x._1))

    val level1MetNulderin = veldjes.filter( {case (f,ps) =>
      ps(0).contains("titleLevel1") && ps.size > 1
    })

    println("Level 1 die level 0 items bevatten:" + level1MetNulderin.size)

    level1MetNulderin.foreach(x => println(x._1))


    val level2MetNulderin = level2Files.filter( {case (f,ps) => ps.size > 1})

    println("Level 2 die level 0 items bevatten:" + level2MetNulderin.size)

    level2MetNulderin.foreach(x => println(x._1))
  }
}

object doubleFields
{

  import PostProcessXML._

  def fixDoubles(d: Elem) =
  	updateElement(d, 
  		e => (e.label == "bibl") &&
        Set("1960", "1970").forall(y =>
          (e \ "interpGrp").exists(
            i => (i \ "@type").text == "witnessYearLevel1_from" && (i \ "interp").text == y)),
  		e => e.copy(child = e.child.filter(c =>
        ! ((  c \ "@type").text == "witnessYearLevel1_from" && (c \ "interp").text == "1970"))))

  	
  def fixFile(in: String, out: String) =
  {
  	val d = fixDoubles(XML.load(in))
  	XML.save(out, d, "UTF-8")
  }

  def doFile(f: java.io.File):List[(String,Int)] =
    {
      val d = XML.loadFile(f)
      val x = (d \\ "interpGrp").map(i => (i \ "@type").toString()).toList.groupBy(identity).mapValues(_.size).toList
      //Console.err.println(x)
      val doubles = x.filter(_._2 > 1)
      if (doubles.nonEmpty) {
        Console.err.println(s"Double interpGrps in ${f.getCanonicalPath}: $doubles")
        val offendingFields = doubles.map(_._1)
        offendingFields.foreach(f => {
          val content = (d \\ "interpGrp").filter(i => (i \ "@type").toString == f)
          Console.err.println(content.map(c => (c \\ "interp").text))
        })
      }
      x
    }

  def main(args: Array[String]): Unit = {
    def it(s: String) =s
    // val allFields = ProcessFolder.processFolder( new java.io.File(args(0)), doFile).flatten
    ProcessFolder.processFolder(new java.io.File(args(0)), new java.io.File(args(1)), fixFile )
    //allFields.foreach(println)
  }
}

object checkDoubleFields
{
  import doubleFields._
  def main(args: Array[String]): Unit = {
    def it(s: String) =s
    val allFields = ProcessFolder.processFolder( new java.io.File(args(0)), doFile).flatten
    allFields.foreach(println)
  }
}
object allPids
{

  def doFile(f: java.io.File):List[String] = (XML.loadFile(f) \\ "interpGrp")
    .filter(i => (i \\ "@type").text == "pid")
    .map(i => (i \ "interp").text.toString())
    .toList

  def main(args: Array[String]): Unit = {
    def it(s: String) =s
    val allFields = ProcessFolder.processFolder( new java.io.File(args(0)), doFile).flatten.toSet.toList.sortBy(it)
    allFields.foreach(println)
  }
}
/*
Hoi,

Ga ik regelen.
Eerst even de metadata af.

Groeten
Jesse

Van: Mathieu Fannee
Verzonden: donderdag 21 juni 2018 13:22
Aan: Jesse de Does
Onderwerp: Dit corpus

Hoi Jesse,

In jouw XML zijn een paar velden niet genoemd volgens de conventie.
Kun je ze als volgt hernoemen?

•         processingmethod_level1  -> processingMethodLevel1
•         processingmethod_level2  -> processingMethodLevel2
•         localization_PlaceLevel0 ->    localization_placeLevel0
•         localization_ProvinceLevel0 ->    localization_provinceLevel0
•         localization_CountryLevel0 ->    localization_countryLevel0
•         localization_KloekecodeLevel0 ->   localization_kloekecodeLevel0
•         localization_RegionLevel0 ->    localization_regionLevel0
•         titlelevel3 ->    titleLevel3
•         subtitlelevel3  ->   subtitleLevel3


Daarnaast zijn er een paar velden die niet in de XML thuishoren. Kun je deze velden wegfilteren uit je export?
•         record_id_level1
•         record_id_level2
•         old_idno
•         sourceref_level1
•         sourceref_level2
•         note

Groeten,
Mathieu

 */