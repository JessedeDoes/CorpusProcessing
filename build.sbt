name := "XmlToRdf"

version := "0.1"

scalaVersion := "2.12.3"

// https://mvnrepository.com/artifact/org.scala-lang.modules/scala-xml_2.12

libraryDependencies += "org.scala-lang.modules" % "scala-xml_2.12" % "1.0.6"

libraryDependencies += "net.sf.saxon" % "Saxon-HE" % "9.8.0-4"

libraryDependencies += "it.unibz.inf.ontop" % "ontop-quest-owlapi" % "1.18.1"

libraryDependencies += "it.unibz.inf.ontop" % "ontop-quest-sesame" % "1.18.1"

resolvers +=  "XQJ Repository" at "http://files.basex.org/maven"

libraryDependencies += "org.basex" % "basex" % "7.6"

resolvers += "XQL Maven Repo" at "http://xqj.net/maven"

libraryDependencies += "net.xqj" % "basex-xqj" % "1.4.0"

libraryDependencies += "com.xqj2" % "xqj2" % "0.1.0"

libraryDependencies += "javax.xml.xquery" % "xqj-api" % "1.0"

// https://mvnrepository.com/artifact/org.json4s/json4s-native_2.12
libraryDependencies += "org.json4s" % "json4s-native_2.12" % "3.5.3"

// https://mvnrepository.com/artifact/javax.servlet/javax.servlet-api
libraryDependencies += "javax.servlet" % "javax.servlet-api" % "4.0.0" % "provided"

enablePlugins(TomcatPlugin)

/*
<dependency>
  <groupId>net.xqj</groupId>
  <artifactId>basex-xqj</artifactId>
  <version>1.2.0</version>
</dependency>
<dependency>
  <groupId>com.xqj2</groupId>
  <artifactId>xqj2</artifactId>
  <version>0.1.0</version>
</dependency>
<dependency>
  <groupId>javax.xml.xquery</groupId>
  <artifactId>xqj-api</artifactId>
  <version>1.0</version>
</dependency>
*/

        
