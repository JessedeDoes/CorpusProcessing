package corpusprocessing.edges.openEdges

import corpusprocessing.edges.openEdges.Alignment.readCorrespondences

import java.io.{File, PrintWriter}
import scala.xml.PrettyPrinter
import scala.xml._
import utils.PostProcessXML
import corpusprocessing.clariah_training_corpora.patch_scripts.fixTokenization.getId

import makeWordIdsGloballyUnique._
case class BibleCorpus(baseDir: String, alignment_files: Set[String], verseAlignedTEIDir: Option[String] = None) {

  lazy val allAlignments: SetOfAlignments =
    SetOfAlignments(this, alignment_files, verseAlignedTEIDir)


  lazy val bibles: List[Bible] = {
    allAlignments.bibles.map{case (l,b) => Bible(this,l,b)}
  }.toList.filter(!_.isEmpty)

  lazy val allBookNames = bibles.flatMap(_.bookNames).toSet.toList.sorted

  def includeBooks(d: Elem, fileName: String) = {
    PostProcessXML.updateElement(d, _.label == "include", x => {
      val href = (x \ "@href").text.replaceAll("content", "ids-fixed") // dit is een beetje suffe hack, moet eruit
      val dir = new File(fileName).getParentFile.getCanonicalPath
      val included = XML.load(dir + "/" + href)
      setAttribute(x.copy(child = included), "href", href)
    })
  }

  def removeBookIncludes(d: Elem)  = {
    PostProcessXML.updateElement(d, _.label == "include", x => {
      x.copy(child = Seq())
    })
  }

  def addWordAlignment(bookname: String, fileName: String, inlineLinks: Boolean = true): Unit = {


    val outputFile = fileName.replaceAll(".xml$", ".wordAligned.xml").replaceAll("alignments", "all-word-alignments")

    Console.err.println("Output to: " + outputFile)

    if (new File(outputFile).exists()) {
      Console.err.println("Already aligned: " + outputFile)
      return ()
    };


    val alignments: List[Alignment] = allAlignments.alignments.map(a => {
      Alignment(a.correspondences.filter(_.book == bookname))
    }).filter(_.nonEmpty).toList

    alignments.foreach(a => println(a.correspondences.size))


    println(Settings.tokenizedContentDir)
    val bookXML = XML.load(fileName)

    val (dBooksIncluded, contentDocuments): (Elem, NodeSeq)  =
      if (true || !inlineLinks) includeBooks(bookXML, fileName) -> List()
      else (bookXML,
        (bookXML \\ "include").map(x => {
          val href = (x \ "@href").text.replaceAll("content", "ids-fixed") // dit is een beetje suffe hack, moet eruit
          val dir = new File(fileName).getParentFile.getCanonicalPath
          val included = XML.load(dir + "/" + href)
          included
        }) )

     // dit is verkeerde logica, beter hele bijbel per talenpaar in plaats van alle taalparen per bijbel

    val dUniqueIds: Elem = dBooksIncluded // makeIdsGloballyUnique(dBooksIncluded)

    val wordAligner = align.fastAlign(dUniqueIds)

    val start = System.currentTimeMillis()

    if (!inlineLinks) {
      val wordAlignmentLayers: Seq[Elem] = alignments.zipWithIndex.map({
        case (a, i) =>
          val start = System.currentTimeMillis()
          println(s"######################################!!!! Start word alignment on book $bookname, at $i of ${alignments.size} alignments")
          val alignmentLayer: Elem = wordAligner.createWordAlignmentLinks(a)
          val end = System.currentTimeMillis()
          println(s"######################################!!!! End word alignment on book $bookname, at $i of ${alignments.size} alignments, took ${end - start} ms")
          alignmentLayer
      }) // .foldLeft(dUniqueIds)({case (e,a) => align.fastAlign.addWordAlignment(e, a)})

      val noIncludes = removeBookIncludes(dUniqueIds)
      val processed = PostProcessXML.updateElement(noIncludes, _.label == "teiCorpus", x => x.copy(child = x.child ++ wordAlignmentLayers))
      // XML.save(outputFile, processed)
      corpusprocessing.corpusutils.SaveHugeXML.save(outputFile, processed)
    } else {
      //case class linkToBible(to_bible: String, word_ids: Seq[String])
      //val bibles = alignments.map(_.bible1).toSet ++ alignments.map(_.bible2).toSet
      this.bibles.foreach(b => {
        println("working on:  " + b.bible)

        val relevantAlignments = alignments.filter(a => Set(a.bible1).contains(b.bible))

        val book: Book = b.books.filter(_.book == bookname).head
        val producedPairs: Seq[(String, Seq[(String, String)])] = relevantAlignments.flatMap(a => {
          val bibleTo = a.bible2
          // println(s"Alignment layer for ${b.bible} $bibleTo $bookname:\n" + a)
          val pairs: Seq[(String, Seq[String])] = wordAligner.makePairs(a)
          val pairsWithTargetBible: Seq[(String, Seq[(String, String)])] = pairs.map{case (id,l) => id -> l.map(bibleTo -> _)}
          pairsWithTargetBible
        })

        // println(producedPairs)

        val mapje: Map[String, Seq[(String, String)]] = producedPairs.groupBy(_._1).mapValues(_.flatMap(_._2))

          // wordAligner.makePairs(a).map(x => linksToBible(a.bible2,x))).groupBy(_._1).mapValues(l => l.map(_._2))
        val fileName = Settings.tokenizedContentDir + "/" + book.xmlFileName
        val bookXML = XML.load(fileName)

        val wordLinkedXML = PostProcessXML.updateElement(bookXML, _.label=="w", w => {
          val id = getId(w)
          val links = mapje.getOrElse(id,Seq()).map({case (bid,wid) => <link type="word-alignment" subtype={bid} target={wid}/>})
          w.copy(child = w.child ++ links)
        })

        XML.save(Settings.wordLinkedContentDir + "/"  + book.xmlFileName, wordLinkedXML)
      })
    }
  }

  def printBooks(toDir: String = "/tmp/Bible/") = {
    new File(toDir).mkdir()
    val subdir = toDir + "/content/"
    new File(subdir).mkdir()

    bibles.foreach(b => {
      val books = b.books
      books.foreach(bk => {
        val f = subdir + bk.xmlFileName
        bk.printTo(f)
      })
    })
  }

  def printBookAlignments(toDir: String = "/tmp/Bible/"): Unit = {
    Console.err.println(s"Collecting alignments for books: $allBookNames")
    new File(toDir).mkdir()
    allBookNames.foreach(bkname => {

      Console.err.println(s"Collecting alignments for book: $bkname")

      new File(toDir).mkdir()
      val books = bibles.map(_.getBook(bkname)).filter(_.nonEmpty).map(_.get)
      val includes = books.map(b => <xi:include href={"../content/" + b.xmlFileName}/>)

      val alignmentInfo: List[Elem] = allAlignments.alignments.map(a => {
        val id = a.bible1 + "--" + a.bible2
        val nCorrespondences = a.filterByBook(bkname).size
        <relation type="verse-alignment" active={a.bible1} passive={a.bible2}><desc>{nCorrespondences} instances</desc></relation>
      }).toList.sortBy(_.toString)

      val allCorrespondences = allAlignments.alignments.map(a => {
        val id = a.bible1 + "--" + a.bible2
        val correspondences = a.filterByBook(bkname)
        <standOff type="book-alignment" xml:id={id}>{correspondences.map(_.toXML)}</standOff>
      })

      val xml = <teiCorpus xmlns:xi="http://www.w3.org/2001/XInclude" xmlns="http://www.tei-c.org/ns/1.0">
        <teiHeader>
          <fileDesc>
            <titleStmt>
              <title>Link file for {bkname}</title>
            </titleStmt>
          </fileDesc>
          <encodingDesc>
            <p><listRelation>{alignmentInfo}</listRelation></p>
          </encodingDesc>
        </teiHeader>
        {includes}
        {allCorrespondences}
      </teiCorpus>
      val subdir = toDir + "/alignments/"
      new File(subdir).mkdir()
      val fileName = subdir + bkname + ".xml"
            // println(formatted)
      val pw = new java.io.PrintWriter(fileName)
      //pw.println(formatted)
      prettyPrinting.prettyScala(pw, xml)
      pw.close()
    })
  }
}
