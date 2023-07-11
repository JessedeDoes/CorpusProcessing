package basex;

import basex.BaseXClient.Query;

import java.io.*;

/**
 * This example shows how queries can be executed in an iterative manner.
 * Iterative evaluation will be slower, as more server requests are performed.
 *
 * This example requires a running database server instance.
 * Documentation: https://docs.basex.org/wiki/Clients
 *
 * @author BaseX Team 2005-23, BSD License
 */
public final class QueryExample {
  /**
   * Main method.
   * @param args command-line arguments
   * @throws IOException I/O exception
   */

  public static void main(final String... args) throws IOException {
    String query = "for $n in //alpino_ds[.//node[@rel='pc']] return " +
            "<result>{$n//sentence} <snippet>{$n//node[@rel='pc']//node[./@word]/ud}</snippet> <snappet>{for $w in $n//node[@rel='pc']//node[./@word] return <w>{$w/@word}</w>}</snappet></result>";
    String database = "LassyEnhanced";
    runQuery(database, query, "/tmp/out.xml");
  }

  public static void runQuery(String database, String input, String toFile) throws IOException {
    // create session
    try(BaseXClient session = new BaseXClient("localhost", 1984, "admin", "admin")) {
      // create query instance


      session.execute("open " + database);

      java.io.PrintWriter pw = new PrintWriter(toFile);
      pw.println("<results>");
      try(Query query = session.query(input)) {
        // loop through all results
        while(query.more()) {
          //val += query.next();
          String r = query.next();
          pw.println(r  + "<!--sepje -->");
        }

        // print query info
        System.err.println(query.info());
      }
      pw.println("</results>");
      pw.close();
    }
  }
}
