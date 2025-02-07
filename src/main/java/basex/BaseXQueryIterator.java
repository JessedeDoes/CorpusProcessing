package basex;

import java.io.IOException;
import java.util.Iterator;
import java.util.NoSuchElementException;
import basex.BaseXClient.Query;

import java.io.*;
import java.util.ArrayList;

public class BaseXQueryIterator implements Iterator<String>, AutoCloseable {
    private final BaseXClient session;
    private final Query query;
    private String nextResult;

    public BaseXQueryIterator(String database, String input) throws IOException {
        this.session = new BaseXClient("localhost", 1984, "admin", "admin");
        this.session.execute("open " + database);
        this.query = session.query(input);
        advance();
    }

    private void advance() {
        try {
            if (query.more()) {
                nextResult = query.next();
            } else {
                nextResult = null;
            }
        } catch (IOException e) {
            throw new RuntimeException("Query execution failed", e);
        }
    }

    @Override
    public boolean hasNext() {
        return nextResult != null;
    }

    @Override
    public String next() {
        if (!hasNext()) {
            throw new NoSuchElementException();
        }
        String result = nextResult;
        advance();
        return result;
    }

    @Override
    public void close() throws IOException {
        query.close();
        session.close();
    }

    public static Iterator<String> getQueryResults(String database, String input) throws IOException {
        return new BaseXQueryIterator(database, input);
    }
}
