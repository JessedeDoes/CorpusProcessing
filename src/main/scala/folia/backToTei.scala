package folia

import scala.xml._
import java.io._
import java.util.zip._

object FoliaToRudimentaryTEI
{
  val xml = "@{http://www.w3.org/XML/1998/namespace}"

  def convert(folia: Node):Node =
  {
    val id = folia \ s"${xml}id"
    val sentences = (folia \\ "s").map(convertSentence)
    <TEI xml:id={id}>
      <teiHeader>
      </teiHeader>
     <text>
       <body>
        <div>
         <p>
           {sentences}
         </p>
        </div>
       </body>
    </text>
    </TEI>
  } 

  def convertSentence(s: Node) = 
  {
    val id = s \ s"${xml}id"
    <s xml:id={id}>
      {(s \\ "w").map(convertWord)}
    </s>
  }
  
  def convertWord(w: Node) = 
  {
    val content = (w \\ "t").filter(t => !((t \ "@class").toString == "contemporary")).text
    val pos =  w \\ "pos" \ "@class"
    val lemma = w \\ "lemma" \ "@class"
    val id = w \ s"${xml}id"

    // scala.Console.err.println(pos)

    val cls = (w \ "@class").toString

    val teiw = if (cls=="WORD") 
          <w xml:id={id} type={pos} lemma={lemma}>{content}</w>
       else
          <pc xml:id={id} type={pos}>{content}</pc>
    
    val space = if ((w \ "@space").text.toString == "NO") Seq() else Seq(Text(" "))
 
    Seq(teiw) ++ space
  }

  def main(args: Array[String])
  {
     val folia = XML.load(new GZIPInputStream(new FileInputStream(args(0))))
     println(convert(folia))
  }
}
