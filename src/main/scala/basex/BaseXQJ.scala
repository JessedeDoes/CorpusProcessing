package basex

import javax.xml.namespace.QName
import javax.xml.xquery.XQException

import net.xqj.basex.BaseXXQDataSource
import javax.xml.xquery.XQDataSource

/*
 * Copyright (c) 2014 xqj.net. All rights reserved.
 */


// look at: https://github.com/BaseXdb/basex/blob/master/basex-examples/src/main/java/org/basex/examples/xqj/XQJQuery.java

/**
  * Basic Example showing how to bind Java values to XQuery external variables
  **/
object BaseXTest {

  private val DRIVER = "net.xqj.basex.BaseXXQDataSource"
  @throws[XQException]
  def test(): Unit = {
    //val ds = new BaseXXQDataSource


    val ds = Class.forName(DRIVER).newInstance.asInstanceOf[XQDataSource]
    import javax.xml.xquery.XQConnection
    val conn = ds.getConnection("admin", "admin")
    ds.setProperty("serverName", "localhost")
    ds.setProperty("port", "1984")
    ds.setProperty("user", "jesse")
    ds.setProperty("password", "dedoes")
    //val conn = ds.getConnection
    val xqe = conn.createExpression

    xqe.executeCommand("SHOW DATABASES")
    xqe.executeCommand("SHOW SESSIONS")
    xqe.executeCommand("SHOW USERS")
    xqe.executeCommand("SHOW EVENTS")
    // http://docs.basex.org/wiki/Commands#OPEN
    xqe.executeCommand("OPEN LassyKlein")
    val exp = conn.prepareExpression("for $node in //node[@rel='whsub'] "
      + "return (<bla>{string-join($node/@word, ' ')}</bla>)")
    val seq = exp.executeQuery
    //seq.getItem().get
    while ( {
      seq.next
    }) System.out.println(seq.getNode.getTextContent)
    conn.close
  }

  def main(args: Array[String]): Unit = test()

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
    while ( {
      rs.next
    }) System.out.println(rs.getItemAsString(null))
    xqc.close
  }
}
