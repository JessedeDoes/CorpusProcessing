package corpusprocessing.MNL

case class Entity(name: String, hex: String, description: Option[String]) {
   lazy val hexClean = hex.replaceAll("[#&x;]","").toLowerCase()
   lazy val  char = Integer.parseInt(hexClean, 16)
   lazy val str =  String.format("%c",char.asInstanceOf[Object])
   def display = s"$name: $hexClean $char $str"
   def isPUA = char >= Integer.parseInt("e000", 16)
}

object Karakters {

   def get[T](r: List[T], i: Int) = if (r.size > i) Some(r(i)) else None
   def readEnts(f: String) = scala.io.Source.fromFile(f).getLines
     .map(_.split("\\t").toList)
     .filter(x => x(1).matches("&#x.*;"))
     .map(r => Entity(r(0), r(1), get(r,2) ))

   val corpus_ents = readEnts("data/MNL/Karakters/mnw.corpus.tab").toList
   val dictionary_ents = readEnts("data/MNL/Karakters/mnw.tab").toList

   lazy val dictionary_chars = dictionary_ents.map(_.char).toSet

   lazy val corp_not_dict = corpus_ents.filter(e => {
      !dictionary_chars.contains(e.char)
   })

   def main(args: Array[String]): Unit = {

      println(corpus_ents.filter(_.isPUA).map(_.display).mkString("\n"))

      println("####")

      println(corp_not_dict.map(_.display).mkString("\n"))
   }
}
