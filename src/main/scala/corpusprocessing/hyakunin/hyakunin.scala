package corpusprocessing.hyakunin

import scala.xml._
import zeeuws.HTML

import java.net.URL
object hyakunin {

  def fromURL(url: String)  = {
    Console.err.println("Get content from:"  + url)
    val lines = io.Source.fromURL(new URL(url)).getLines()
    // Console.err.println(lines.toList)
    val content = lines.mkString("\n")
    val contentX = HTML.parse(content) \\ "article"
    contentX
  }

  case class Item(tr: Node)  {
    lazy val cols = tr.descendant.filter(x  => Set("td", "th").contains(x.label))
    lazy val n = cols(0).text
    lazy val title = cols(1).text
    lazy val author = cols(2).text
    lazy val link: String = (tr \\ "a" \ "@href").text.trim
    lazy val content = fromURL(link)
  }

  val list = "https://ogurasansou.jp.net/columns_category/hyakunin/"
  val example = "https://ogurasansou.jp.net/columns/hyakunin/2017/10/18/17/"
  lazy val index = fromURL(list)
  lazy val x = fromURL(example)
  lazy val items = (index \\ "tr").map(Item).toList.drop(1)

  lazy val doc = <html>
    <body>
       {items.map(i => {
         <div>
           <h1>{i.n}: {i.title}</h1>
           {
             i.content
           }
         </div>
        })
      }
    </body>
  </html>
  def main(args: Array[String]): Unit = {
    // println(index.text)
    items.foreach(x => println(s"${x.n}\t${x.title}\t${x.link}"))
    println(items.head.content)
    XML.save("/tmp/hyakunin.html", doc)
  }
}
