package corpusprocessing.wolkencorpus

import java.io.IOException
import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes

object renameFilesInZip {

  protected def rezip(zipfile: String, olddir: String, newdir: String): Unit = {
    val zipFilePath = Paths.get(zipfile)

    try {
      val fs = FileSystems.newFileSystem(zipFilePath, null)
      try {
        val oldpathInsideZipPath = fs.getPath(olddir)
        if (!Files.exists(Paths.get(newdir))) Files.createDirectory(Paths.get(newdir))
        if (Files.exists(oldpathInsideZipPath, LinkOption.NOFOLLOW_LINKS)) Files.walkFileTree(oldpathInsideZipPath, new SimpleFileVisitor[Path]() {
          @throws[IOException]
          override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
            if (file.toString.indexOf(olddir) > -1) {
              val a = file.toString.replaceAll(olddir, newdir)
              val b = fs.getPath(a)
              if (!Files.exists(b.getParent)) Files.createDirectories(b.getParent)
              Files.move(file, b, LinkOption.NOFOLLOW_LINKS)
            }
            FileVisitResult.CONTINUE
          }

          override def postVisitDirectory(dir: Path, e: IOException): FileVisitResult = if (e == null) {
            Files.delete(dir)
            FileVisitResult.CONTINUE
          }
          else { // directory iteration failed
            throw e
          }
        })
        fs.close()
      } catch {
        case e: Exception =>
          e.printStackTrace()
      } finally if (fs != null) fs.close()
    }
  }

  def main(args: Array[String]): Unit = {
    rezip(args(0), args(1), args(2))
  }
}
