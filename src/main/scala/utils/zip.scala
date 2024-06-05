package utils

import java.io.OutputStreamWriter
import java.net.URI
import java.nio.file.{Path, _}

import scala.collection.JavaConversions._

// let op: KB krantenzips hebben een javaprobleempje als ze groter dan 4G zijn.
// java.util.zip ZipException: loc: wrong sig
// fix ze dan met  7z a 180x.zip dinges.txt

object zipUtils
{
  val testFile = "data/kranten_pd_voorbeeld.zip"

  @throws[Exception]
  def getRootPath(zipFile: String): Path =
  {
    val env = new java.util.HashMap[String,String]()
    val absolutePath = new java.io.File(zipFile).getAbsolutePath
    env.put("create", "true")
    val uri = URI.create(s"jar:file:$absolutePath")
    val zipfs:FileSystem = try {
      FileSystems.getFileSystem(uri)
    }
    catch {
      case e: FileSystemNotFoundException => FileSystems.newFileSystem(uri, env)
    }
    val path:Path = zipfs.getPath("/")
    path
  }


  def find(zipFile: String, filter: Path => Boolean=truth):Stream[Path] =
    try { findPath(getRootPath(zipFile), filter) } catch
      {
        case e:Exception =>
          Console.err.println(s"Failure opening $zipFile")
          e.printStackTrace()
          Stream[Path]()
      }

  def truth(p:Path):Boolean = true

  @throws[Exception]
  def findPath(path:Path, filter: Path => Boolean=truth):Stream[Path] =
  {
    try {
    if (Files.isDirectory(path))
    {
      val children = Files.newDirectoryStream(path).toStream
      val down = children.flatMap(d => findPath(d,filter))
      down
    }
    else {
      if (filter(path)) new Stream.Cons(path, Stream.empty[Path]) else Stream.empty[Path]
    }
    } catch {

      case e:Exception =>
        Console.err.println(e)
        e.printStackTrace()
        Stream.empty[Path]
    }
  }

  def getPath(root: Path, filename: String):Path =
  {
    val fs = root.getFileSystem
    val pInZip:Path = fs.getPath(root.toString, filename)
    val parent:Path = pInZip.getParent
    if (Files.notExists(parent))
    {
      Console.err.println("Create:" + parent)
      Files.createDirectories(parent)
    }
    pInZip
  }

  def getOutputStream(root: Path, filename: String): java.io.OutputStream =
  {
    val fs = root.getFileSystem
    val pInZip:Path = fs.getPath(root.toString, filename)
    val parent:Path = pInZip.getParent
    if (Files.notExists(parent))
    {
      Console.err.println("Create:" + parent)
      Files.createDirectories(parent)
    }
    if (Files.exists(pInZip))
    {
      System.err.println("Oh jee")
      Files.delete(pInZip)
    }
    Files.newOutputStream(pInZip)
  }

  def getWriter(root: Path, filename: String): OutputStreamWriter = new OutputStreamWriter(getOutputStream(root,filename))


  def main(args: Array[String]) =
  {
    val root:Path = getRootPath("/tmp/test.zip")

    Console.err.println(root.getFileSystem.isReadOnly)

    val p = Paths.get("Documentation/EE3.5.doc")
    val fs = root.getFileSystem
    val pInZip:Path = fs.getPath(root.toString, "/aap/noot/mies/my.new.file.txt")
    val parent:Path = pInZip.getParent

    if (Files.notExists(parent))
      {
        Console.err.println("Create:" + parent)
        Files.createDirectories(parent)
      }

    //Files.
    //Files.newOutputStream()

    val os = new OutputStreamWriter(Files.newOutputStream(pInZip))
    os.write("happy little dog")
    os.flush()
    os.close()
    root.getFileSystem.close
    //findPath(path, p => p.endsWith("didl.xml")).foreach(println)
  }
}
