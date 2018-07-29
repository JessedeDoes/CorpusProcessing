name := "XmlToRdf"

version := "0.1"

scalaVersion := "2.12.3"

// https://mvnrepository.com/artifact/org.scala-lang.modules/scala-xml_2.12

libraryDependencies += "org.scala-lang.modules" % "scala-xml_2.12" % "1.0.6"

// https://mvnrepository.com/artifact/org.scala-lang.modules/scala-parser-combinators_2.12
libraryDependencies += "org.scala-lang.modules" % "scala-parser-combinators_2.12" % "1.0.6"


libraryDependencies += "mysql" % "mysql-connector-java" % "5.1.16" force()
libraryDependencies += "org.jdbi" % "jdbi" % "2.78"
libraryDependencies += "org.postgresql" % "postgresql" % "42.1.1"

// https://mvnrepository.com/artifact/org.incava/java-diff
libraryDependencies += "org.incava" % "java-diff" % "1.1"


libraryDependencies += "net.sf.saxon" % "Saxon-HE" % "9.8.0-4"

libraryDependencies += "it.unibz.inf.ontop" % "ontop-quest-owlapi" % "1.18.1"

libraryDependencies += "it.unibz.inf.ontop" % "ontop-quest-sesame" % "1.18.1"

resolvers +=  "Basex Repository" at "http://files.basex.org/maven"

libraryDependencies += "org.basex" % "basex" % "8.6.7"

resolvers += "XQL Maven Repo" at "http://xqj.net/maven"

libraryDependencies += "net.xqj" % "basex-xqj" % "1.4.0"

libraryDependencies += "com.xqj2" % "xqj2" % "0.2.0"

libraryDependencies += "javax.xml.xquery" % "xqj-api" % "1.0"

// https://mvnrepository.com/artifact/org.json4s/json4s-native_2.12
libraryDependencies += "org.json4s" % "json4s-native_2.12" % "3.5.3"

// https://mvnrepository.com/artifact/javax.servlet/javax.servlet-api
libraryDependencies += "javax.servlet" % "javax.servlet-api" % "4.0.0" % "provided"

// https://mvnrepository.com/artifact/org.ow2.sat4j/org.ow2.sat4j.core

libraryDependencies += "org.ow2.sat4j" % "org.ow2.sat4j.core" % "2.3.5"

// https://mvnrepository.com/artifact/guru.nidi/graphviz-java
libraryDependencies += "guru.nidi" % "graphviz-java" % "0.5.2"

enablePlugins(TomcatPlugin)

assemblyMergeStrategy in assembly := {
    case x if Assembly.isConfigFile(x) =>
      MergeStrategy.concat
    case PathList(ps @ _*) if Assembly.isReadme(ps.last) || Assembly.isLicenseFile(ps.last) =>
      MergeStrategy.rename
    case PathList("META-INF", xs @ _*) =>
      (xs map {_.toLowerCase}) match {
        case ("manifest.mf" :: Nil) | ("index.list" :: Nil) | ("dependencies" :: Nil) =>
          MergeStrategy.discard
        case ps @ (x :: xs) if ps.last.endsWith(".sf") || ps.last.endsWith(".dsa") =>
          MergeStrategy.discard
        case "plexus" :: xs =>
          MergeStrategy.discard
        case "services" :: xs =>
          MergeStrategy.filterDistinctLines
        case ("spring.schemas" :: Nil) | ("spring.handlers" :: Nil) =>
          MergeStrategy.filterDistinctLines
        case _ => MergeStrategy.first
      }
    case _ => MergeStrategy.first
  }

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

        
