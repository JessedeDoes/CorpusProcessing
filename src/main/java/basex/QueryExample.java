package basex;

import basex.BaseXClient.Query;

import java.io.*;
import java.util.ArrayList;

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




  public static ArrayList<String> runQuery(String database, String input, String toFile) throws IOException {
    // create session
    ArrayList<String> l = new ArrayList<>();
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
          l.add(r);
          pw.println(r  + "\n<!--result separator-->");
        }

        // print query info
        System.err.println(query.info());
      }
      pw.println("</results>");
      pw.close();

    } catch (Exception e) {
      e.printStackTrace();
      return l;
    }
    return l;
  }

  public static ArrayList<String> getQueryResults(String database, String input) throws IOException {
    // create session
    ArrayList<String> l = new ArrayList<>();
    try(BaseXClient session = new BaseXClient("localhost", 1984, "admin", "admin")) {
      // create query instance


      session.execute("open " + database);





      try(Query query = session.query(input)) {
        // loop through all results

        while(query.more()) {
          //val += query.next();
          String r = query.next();
          l.add(r);

        }

        // print query info
        System.err.println(query.info());
      }


    } catch (Exception e) {
      e.printStackTrace();
      return l;
    }
    return l;
  }
}
