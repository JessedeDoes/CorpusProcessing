package db2rdf

import java.io.{FileOutputStream, FileWriter, OutputStreamWriter}
import java.sql.ResultSet
import java.util.zip.GZIPOutputStream

import db2rdf.Settings.gigantHilexDB
import db2rdf.commonDefinitions.diamantGraph

object exportDiamantToRDF {


  import Settings._

  val limit = Int.MaxValue

  var memMax:Long = 0
  def mem(): Unit =
  {
    val heapSize = Runtime.getRuntime.totalMemory
    if (heapSize > memMax + 1e5)
    {
      Console.err.println(s"$heapSize")
      memMax = heapSize
    }
  }

  import diamantMapping._

  def main(args: Array[String]) =
  {
    import org.slf4j.Logger
    import org.slf4j.LoggerFactory

    val outputFolder = if (args.length > 0) args(0) else "/tmp/"

    System.setProperty("org.slf4j.simpleLogger.logFile", "/tmp/loggertje.log")
    System.setProperty("org.slf4j.simpleLogger.defaultLogLevel", "off")
    System.setProperty("log4j.rootLogger", "WARN, file")

    //val logger = LoggerFactory.getLogger(classOf[Nothing])
    //logger.info("Hello World")

    gigantHilexDB.runStatement(("set schema 'data'"))

    //Console.err.println(s"######################################################lemmata en PoS voor ${lemmata.triplesIterator(db, lemmaQuery).size} lemmata")

    def write(m: Mappings, w: java.io.Writer, graph:IRI = diamantGraph): Unit =
    {
      m.triplesIterator(gigantHilexDB).map(x => x.withGraph(graph)).filter(_.valid()).foreach(x => w.write(x.toString + "\n"))
    }

    val distinctPosOutput = new OutputStreamWriter(new GZIPOutputStream( new FileOutputStream(outputFolder + "/" + "poslist.nq.gz")))
    write(distinctPosMapping, distinctPosOutput)
    distinctPosOutput.close()
    // System.exit(0)


    val lemmaOutput = new OutputStreamWriter(new GZIPOutputStream(new FileOutputStream(outputFolder + "/" + "lemmata.nq.gz")))
    val wordformOutput = new OutputStreamWriter(new GZIPOutputStream( new FileOutputStream(outputFolder + "/" + "wordforms.nq.gz")))
    val senseOutput = new OutputStreamWriter(new GZIPOutputStream( new FileOutputStream(outputFolder + "/" + "senses.nq.gz")))
    val attestationOutput = new OutputStreamWriter(new GZIPOutputStream( new FileOutputStream(outputFolder + "/" + "attestations.nq.gz")))
    val ezelOutput = new OutputStreamWriter(new GZIPOutputStream( new FileOutputStream(outputFolder + "/" + "ezels.nq.gz")))
    val serpensOutput = new OutputStreamWriter(new GZIPOutputStream( new FileOutputStream(outputFolder + "/" + "serpens.nq.gz")))




    write(lemmata, lemmaOutput)
    write(posMapping, lemmaOutput)

    //lemmata.triplesIterator(db, lemmaQuery).take(limit).foreach(x => lemmaOutput.write(x.toString + "\n"))
    // posMapping.triplesIterator(db, posQuery).take(limit).foreach(x => lemmaOutput.write(x.toString + "\n"))

    lemmaOutput.close()

    //Console.err.println(s"###################################################### woordvormen ${lemmaWordform.triplesIterator(db, wordformQuery).size} triples")

    write(lemmaWordform, wordformOutput)

    //lemmaWordform.triplesIterator(db, wordformQuery).take(limit).foreach(x => wordformOutput.write(x.toString + "\n"))

    wordformOutput.close()

    Console.err.println(s"###################################################### senses synonym stuk: ${synonyms.triplesStream(gigantHilexDB, synonymQuery).size}")

    write(senses,senseOutput)
    write(subsenses, senseOutput)
    write(synonyms, senseOutput)

    //senses.triplesIterator(db, senseQuery).take(limit).foreach(x => senseOutput.write(x.toString + "\n"))
    // synonyms.triplesIterator(db, synonymQuery).take(limit).foreach(x => senseOutput.write(x.toString + "\n"))

    senseOutput.close()


    Console.err.println("###################################################### attestations en quotations")


    write(quotations, attestationOutput)
    write(senseAttestations, attestationOutput)
    write(attestations, attestationOutput)

    //quotations.triplesIterator(db, documentQuery).take(limit).foreach(x => attestationOutput.write(x.toString + "\n"))
    //senseAttestations.triplesIterator(db, senseAttestationQuery).take(limit).foreach(x => attestationOutput.write(x.toString + "\n"))
    //attestations.triplesIterator(db, attestationQuery).take(limit).foreach(x => attestationOutput.write(x.toString + "\n"))

    attestationOutput.close()

    Console.err.println("###################################################### ezels")

    write(hilexSynsets, ezelOutput)
    write(hilexSynsetRelations, ezelOutput)

    //hilexSynsets.triplesIterator(db, synsetQuery).take(limit).foreach(x => ezelOutput.write(x.toString + "\n"))
    //hilexSynsetRelations.triplesIterator(db, synsetRelationQuery).take(limit).foreach(x => ezelOutput.write(x.toString + "\n"))

    ezelOutput.close()

    Console.err.println("###################################################### serpens")

    write(serpensConcepts, serpensOutput)
    write(serpensWNT, serpensOutput)
    write(serpensConceptRelations, serpensOutput)

    //serpensConcepts.triplesIterator(db, serpensConceptQuery).take(limit).foreach(x => serpensOutput.write(x.toString + "\n"))
    //serpensWNT.triplesIterator(db, serpensWntQuery).take(limit).foreach(x => serpensOutput.write(x.toString + "\n"))
    //serpensConceptRelations.triplesIterator(db, conceptRelationQuery).take(limit).foreach(x => serpensOutput.write(x.toString + "\n"))

    serpensOutput.close()

  }
}
