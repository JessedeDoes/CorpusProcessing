package corpusprocessing.clariah_training_corpora.going_dutch

import utils.HTML
import scala.xml._
import java.io.{File, PrintWriter}
import utils.PostProcessXML._

import scala.util.{Failure, Success, Try}
object GoingDutch {

   val plainTextFolder = "/mnt/Projecten/Corpora/Historische_Corpora/GoingDutch/FinalSelection"
   val  xmlFolder = "/mnt/Projecten/Corpora/Historische_Corpora/GoingDutch/FinalSelection/XML/"
   lazy val files = new File(plainTextFolder).listFiles().filter(_.getName.endsWith("txt"))

   def toInlMetadata(fields: Map[String,String])  = {

      def f(x:String) = fields.getOrElse(x,"")

      <bibl>
        <interpGrp type="corpusProvenance"><interp>Going Dutch</interp></interpGrp>
        <interpGrp type="editorLevel3"><interp>Andreas Krogull</interp></interpGrp>
        <interpGrp type="titleLevel2"><interp>{f("title")}</interp></interpGrp>
        <interpGrp type="titleLevel1"><interp>{f("title")}</interp></interpGrp>
        <interpGrp type="witnessYearLevel1_from"><interp>{f("year")}</interp></interpGrp>
        <interpGrp type="witnessYearLevel1_to"><interp>{f("year")}</interp></interpGrp>
        <interpGrp type="witnessYearLevel2_from"><interp>{f("year")}</interp></interpGrp>
        <interpGrp type="witnessYearLevel2_to"><interp>{f("year")}</interp></interpGrp>
        <interpGrp type="decade"><interp>{f("decade")}</interp></interpGrp>
      </bibl>
   }

   def parseHeader(h: Elem, fileName: String) =  {
     val fn = new File(fileName).getName
     val lines = h.text.split("\\n").toList.filter(_.contains(":"))
     val fields = lines.map(l => l.split("\\s*:\\s*",-1)).map(a => a(0).trim -> a(1).trim).toMap ++ Map("title" -> fn)

     val extra = if (fields.contains("DATE") && fields("DATE").matches(".*[0-9]{4}.*")) {
       val year = fields("DATE").replaceAll("^.*?([0-9]{4}).*", "$1")
       val decade = year.replaceAll(".$", "0")
       val centuryFloor = year.replaceAll("..$", "00").toInt
       val century = s"$centuryFloor--${centuryFloor+100}"
       Map("year" -> year, "decade" -> decade, "century" -> century)
     }  else Map()

     val interps = (fields ++ extra).map({case (n,v)  => <interpGrp type={n.toLowerCase()}><interp>{v}</interp></interpGrp>})

     val inlMetadata = toInlMetadata(fields ++ extra)


     <teiHeader>
       <fileDesc>
         <titleStmt>
           <title>fn</title>
           <respStmt>
             <resp>compiled by</resp>
             <name>Andreas Krogull</name>
           </respStmt>
         </titleStmt>
         <publicationStmt>
           <availability><licence>This file may not be redistributed!! It is a preliminary version</licence></availability>
         </publicationStmt>
       </fileDesc>
       <sourceDesc>
         <listBibl type="goingDutchMetadata">
           <bibl>
             {interps}
             </bibl>
           </listBibl>
         <listBibl type="inlMetadata">

             {inlMetadata}

         </listBibl>
         </sourceDesc>
     </teiHeader>
   }

   def parseFile(f: String): Unit =  {
     Try {
       val content = "<TEI>" + io.Source.fromFile(f,enc = "iso-8859-1").getLines().mkString("\n") + "</TEI>"
       val rawDoc = HTML.parse(content).asInstanceOf[Elem]
       val doc1 = updateElement(rawDoc, _.label == "header", x => parseHeader(x,f))
       val doc2 = {
         val children = doc1.child.filter(_.label != "teiHeader")
         <TEI>
           {doc1 \ "teiHeader"}
         <text>
           <body>
             <div>
               {children}
             </div>
           </body>
         </text>
         </TEI>
       }
       val pretty = new scala.xml.PrettyPrinter(300, 4)
       val outFile = xmlFolder + "/" + new File(f).getName.replaceAll(".txt$", ".xml")
       val writer = new PrintWriter(outFile)
       writer.print(pretty.format(doc2))
       writer.close()
       //print(doc1)
     } match {
       case Success(x) => x
       case Failure(exception) => Console.err.println(exception); <bummer/>
     }
   }

   def main(args: Array[String])  = {
     files.foreach(x => parseFile(x.getCanonicalPath))
   }
}
