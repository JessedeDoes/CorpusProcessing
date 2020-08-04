package utils

import java.io.File

object ProcessFolder {

  def processFolder[T](input: File, action: File => T):Seq[T] =
  {

    if (input.isDirectory) Console.err.println(input.listFiles().toList)
    if (input.isFile)
      Stream(action(input))
    else input.listFiles.toList.flatMap(x => processFolder(x,action) )
  }

  def filesIn(input: File) = processFolder[File](input, identity[File])

  def processFolder(input: File, outputFolder: File, base: (String,String) => Unit, parallel: Boolean = true): Unit =
  {
    if (!outputFolder.exists())
      outputFolder.mkdir()

    if (input.isDirectory)
    {
      val z = if (parallel) input.listFiles().toList.par else input.listFiles().toList
      z.foreach(f =>
      {
        if (f.isFile && (f.getName.endsWith(".xml") || f.getName.endsWith(".xml.gz")))
          processFolder(f, outputFolder, base)
        else if (f.isDirectory)
          processFolder(f, new File(outputFolder + "/" + f.getName), base)
      })
    } else if (input.isFile)
    {
      //Console.err.println(input.getName)
      val outFile = outputFolder + "/" + input.getName()
      base(input.getCanonicalPath, outFile)
    }
  }
}