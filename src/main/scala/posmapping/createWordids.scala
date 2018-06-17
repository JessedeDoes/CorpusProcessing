package posmapping

import scala.xml._

object createWordids {
  def updateElement(e: Elem, condition: Elem => Boolean, f: Elem => Elem): Elem = {
    if (condition(e))
      f(e)
    else
      e.copy(child = e.child.map({
        {
          case e1: Elem => updateElement(e1, condition, f)
          case n: Node => n
        }
      }))
  }

  def updateElement3(e: Elem, condition: Elem=>Boolean, f: Elem => Elem):Elem =
  {
    val bla:Seq[Node] = e.child.map({
      case e1: Elem => updateElement3(e1,condition,f).asInstanceOf[Node]
      case n: Node => n.asInstanceOf[Node]
    })

    if (condition(e))
      f(e.copy(child=bla))
    else
      e.copy(child = bla)
  }

  def createNumberedWordIds(node: Node, n: Int): (Node, Int) = {
    val id = new PrefixedAttribute("xml", "id", s"w.$n", Null)

    node match {
      case w: Elem if (w.label == "w" || w.label == "pc") =>
        (w.copy(attributes = w.attributes.append(id)), n + 1)

      case e: Elem if e.child.nonEmpty =>

        val c0 = createNumberedWordIds(e.child.head, n)

        if (e.child.tail.nonEmpty) {
          def step(b: Seq[(Node, Int)], x: Node): Seq[(Node, Int)] = {
            val z = createNumberedWordIds(x, b.last._2)
            b ++ Seq(z)
          }

          val zz = e.child.tail.foldLeft(Seq(c0))(step)
          val newChild = zz.map(_._1)
          val newN = zz.last._2
          (e.copy(child = newChild), newN)
        } else  {
          val newChild = c0._1
          val newN = c0._2
          (e.copy(child = newChild), newN)
        }

      case x: Node => (x, n)
    }
  }

  def doDivOfPee(e: Elem):Elem =
  {
    if (e.label.startsWith("div")) e.copy(child = <ab>{e.child}</ab>)
    else if (e.label == "p") e.copy(label = "s")
    else e
  }

  def createWordIds(d: Elem) =
    {
      val d1 = createNumberedWordIds(d, 0)._1.asInstanceOf[Elem]
      updateElement3(d1, x => x.label.startsWith("div") || x.label=="p", doDivOfPee)
    }


  def testje() =
  {
    val x = <text> <w/> <w/> <div><p> <w/><w/></p></div> <w/> <w/></text>
    println(createWordIds(x))
  }

  def doit(f1: String, f2:String) = XML.save(f2, createWordIds(XML.load(f1)), "UTF-8")

  def main(args: Array[String]): Unit = {

    utils.ProcessFolder.processFolder(new java.io.File(args(0)), new java.io.File(args(1)), doit)
  }
}
