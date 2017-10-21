package basex

import scala.xml._
import javax.xml.namespace.QName
import javax.xml.xquery.XQException

import net.xqj.basex.BaseXXQDataSource
import javax.xml.xquery.XQDataSource
import javax.xml.xquery.{XQItem, XQSequence}

import scala.collection.JavaConversions._
import javax.xml.xquery.XQConnection

import scala.annotation.tailrec


case class BaseXConnection(server: String, port: String, user: String, password: String, database: String)
{
  lazy val ds = Class.forName(DRIVER).newInstance.asInstanceOf[XQDataSource]
  lazy val connection =
  {
    ds.setProperty("serverName", "localhost")
    ds.setProperty("port", "1984")
    ds.setProperty("user", "jesse")
    ds.setProperty("password", "dedoes")
    val c = ds.getConnection
    val xqe = c.createExpression
    xqe.executeCommand(s"OPEN $database")
    c
  }

  //val x = XML.loadString("aap")

  //danger: stack overflow
  final def toStream(seq: XQSequence):Stream[XQItem] =
  {
    if (!seq.next)
      Stream.empty[XQItem]
    else
      Stream.cons(seq.getItem, toStream(seq))
  }

  def getAsNodes(query: String):Stream[org.w3c.dom.Node] =
    getQuery(query).map(_.getNode)

  def getAsScalaNodes(query: String):Stream[scala.xml.Node] =
    getQuery(query).map(item => XML.loadString(item.getItemAsString(null)))

  def getQuery(query: String):Stream[XQItem] =
  {
    val exp = connection.prepareExpression(query)
    val seq = exp.executeQuery()
    toStream(seq)
  }
  private val DRIVER = "net.xqj.basex.BaseXXQDataSource"
}

object BaseXTest
{
  private val DRIVER = "net.xqj.basex.BaseXXQDataSource"
  @throws[XQException]
  def test(): Unit = {
    //val ds = new BaseXXQDataSource
    val ds = Class.forName(DRIVER).newInstance.asInstanceOf[XQDataSource]

    //val conn = ds.getConnection("admin", "admin")
    ds.setProperty("serverName", "localhost")
    ds.setProperty("port", "1984")
    ds.setProperty("user", "jesse")
    ds.setProperty("password", "dedoes")
    val conn = ds.getConnection
    val xqe = conn.createExpression


    // http://docs.basex.org/wiki/Commands#OPEN
    xqe.executeCommand("OPEN LassyKlein")
    val exp = conn.prepareExpression("for $node in //node[@cat='whsub'] "
      + "return (<bla>{string-join($node//@word, ' ')}</bla>)")
    val seq = exp.executeQuery
    //seq.getItem().get
    while (seq.next)
    {
      System.out.println(seq.getNode.getTextContent)
    }
    conn.close
  }

  def test1(): Unit =
  {
    val p = new scala.xml.PrettyPrinter(80,2)

    val x = BaseXConnection(server="localhost", port="1984", user="jesse", password="dedoes",
      database="LassyKlein")
    val q = "for $node in //node[@cat='whsub'] " +
      "return (<bla>{string-join($node//@word, ' ')}</bla>)"

    x.getAsScalaNodes(q).foreach(x => println(p.format(x)))
  }
  def main(args: Array[String]): Unit = test1()
}


/*
 * Copyright (c) 2014 xqj.net. All rights reserved.
 */

// look at: https://github.com/BaseXdb/basex/blob/master/basex-examples/src/main/java/org/basex/examples/xqj/XQJQuery.java

/**
  * Basic Example showing how to bind Java values to XQuery external variables
  **/


 /*
 def xmain(args: Array[String]): Unit = {
   if (args.length != 4) {
     System.out.println("usage: java BindingVariables " + "host port user password")
     return
   }
   val ds = new BaseXXQDataSource
   ds.setProperty("serverName", args(0))
   ds.setProperty("port", args(1))
   ds.setProperty("user", args(2))
   ds.setProperty("password", args(3))
   val xqc = ds.getConnection
   val xqpe = xqc.prepareExpression("declare variable $str as xs:string external;\n" + "declare variable $doc as document-node(element(e)) external;\n" + "fn:concat('String: ', $str),\n" + "fn:concat('Document: ', fn:serialize($doc))")
   // ... Bind String ...
   xqpe.bindString(new QName("str"), "Hello World", null)
   // ... Bind Document ...
   xqpe.bindDocument(new QName("doc"), "<e>Hello World</e>", null, null)
   val rs = xqpe.executeQuery
   while (rs.next) System.out.println(rs.getItemAsString(null))
   xqc.close
 }
 */