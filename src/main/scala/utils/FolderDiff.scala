package utils

import java.io.File

object FolderDiff {

  val logje = new java.io. PrintWriter("/tmp/comparefolder.logje.taalbank.out")
  def compareFolders(f1: File, f2: File): Unit = {
    // Console.err.println(s"$f1 ---- $f2")
    if (!f2.exists()) {
      println(s"$f1 is missing!")
      return
    }

    if (f1.isDirectory) {
      val l1 = f1.listFiles().toList
      val l2 = f2.listFiles().toList
      val m1 = f1.listFiles().map(x => x.getName -> x).toMap
      val m2 = f2.listFiles().map(x => x.getName -> x).toMap

      l1.filter(_.isFile).foreach(f1 => {
        val two = m2.get(f1.getName)
        two.foreach(t => {
          val m1 = f1.lastModified()
          val m2 = t.lastModified()
          if (m1  > m2){
            logje.println(s"$f1 NEWER-THAN $t!!")
            logje.flush()
          } else {
            // println(s"Krek $m1 $m2")
          }
        })
        if (two.isEmpty) {
          logje.println(s"F1-F2: $f1")
          logje.flush()
        }
      })
      l2.foreach(f2 => {if (!m1.contains(f2.getName))
        logje.println(s"F2-F1: $f2")
        logje.flush()
      })
      l1.filter(_.isDirectory).foreach(d1 => {
        val d2 = new File(f2.getCanonicalPath + "/" + d1.getName)
        compareFolders(d1,d2)
      })
    }
  }

  val c12 = new File("/mnt/Projecten12/CLARIAH/CLARIAH-PLUS")
  val c5 = new File("/mnt/Projecten/CLARIAH/CLARIAH-PLUS")

  val p12 = new File("/mnt/Projecten/Taalbank")
  val p5 = new File("/mnt/Projecten05/Taalbank")

  def main(args: Array[String]): Unit = {
    compareFolders(p5, p12)
    logje.close()
  }
}
