def capFirst(s: String) = s.substring(0,1).toUpperCase + s.substring(1, s.length)

// data/namenKlus.preproc
val poging = scala.io.Source.fromFile("data/namenKlus.preproc").getLines.toStream.map(l => l.split("\\s*->\\s*")).filter(_.size>1).map(l => l(0) -> l(1)).toMap

scala.io.Source.fromFile(args(0)).getLines.toStream.map(l => l.split("\\s*->\\s*")).filter(_.size>1).foreach(
  l => {
     val left =l(0).trim; 
     val right = l(1).replaceAll("_","").trim
     val p = poging.get(left)
     Console.err.println(s"|$right| |$p|")
     val newRight = if (poging.contains(left)) poging(left) else 
       if (right.nonEmpty) right else left.split("-").map(capFirst).mkString(" ")

     println(s"$left -> $newRight")
    }
  ) 
