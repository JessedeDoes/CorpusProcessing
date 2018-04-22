package utils;

/*
 * Compute alignments for a weighted transducer:
 * Aligner for he joint unigram case ("learning string edit distance")
 * This is just the usual string edit distance procedure
 */

import java.util.*;

public class Aligner {

    static final int REPLACE = 0;
    static final int INSERT = 1;
    static final int DELETE = 2;
    static final int NOOP = 4;

    static final char nul = 0;

    private double[][] costMatrix;
    int n, m;
    private int[][] operationMatrix;

    public static class Chunk
    {
        public String left="";
        public String right="";
        public int position;
        double cost=0;
        int type;

        public Chunk(char x, char y, int p)
        {
            position  = p;
            if (x != 0)
                left += x;
            if (y != 0)
                right += y;

            if (x == y) type = NOOP ;
            else if (x != 0 && y != 0)
            {type = REPLACE ; cost = replaceCost(x,y); }
            else if (x == 0) {
                type = INSERT ;
                cost = insertCost(y);
            }
            else {
                type = DELETE;
                cost = deleteCost(x);
            }
        }

        public Chunk(String x, String y, int p)
        {
            position  = p;
            left = "" + x;
            right = "" + y;
            if (x.equals(y)) type = NOOP ;
            else if (!x.isEmpty() && !y.isEmpty()) type = REPLACE ;
            else if (x.isEmpty()) type = INSERT ;
            else type = DELETE;
        }

        public String toString()
        {
            return type + ":[" + left + ":" + right + "]" ;
        }

        public Chunk add(Chunk next)
        {
            Chunk n = new Chunk(this.left + next.left, this.right + next.right, this.position);
            return n;
        }

        public void append(Chunk next)
        {
            Chunk n = new Chunk(this.left + next.left, this.right + next.right, this.position);
            this.left  = n.left;
            this.right = n.right;
            this.type = n.type;
        }
    }

    public static List<Chunk> clunk(List<Chunk> in)
    {
        ArrayList<Chunk> x = new ArrayList<>();
        x.add(in.get(0));
        for (int i=1; i < in.size(); i++)
        {
            Chunk prev = x.get(x.size()-1);
            Chunk cur = in.get(i);
            if (prev.type == NOOP && cur.type == NOOP)
                prev.append(cur);
            else if (prev.type != NOOP && cur.type != NOOP)
                prev.append(cur);
            else
                x.add(cur);
        }
        return x;
    }


    public Aligner() {
    }


	/*
     * Does Math.max handle negative infinity correctly?
	 */

    double best(double x, double y) {
        double r = Math.min(x, y);
        if (r == Double.NaN) {
            //org.ivdnt.openconvert.log.ConverterLog.defaultLog.printf("Huh\n");
            System.err.println("Nee he!");
            return Double.POSITIVE_INFINITY;
        }
        return r;
    }

    static double deleteCost(char x) {
        return 1;
    }

    static double insertCost(char x) {
        return 1;
    }

    static double replaceCost(char x, char y) {
        if (x == y) {

            return 0;
        }
        return 1.2;
    }

    double getCost(int a, int b) {
        return costMatrix[a][b];
    }


    double newCost(double oldCost, double delta) {
        return oldCost + delta;
    }

    int getOperation(int a, int b) {
        return operationMatrix[a][b];
    }

    void setCost(int a, int b, double d) {
        if (Double.isNaN(d)) {
            System.err.printf("NaN at %d %d\n", a, b);
        }
        costMatrix[a][b] = d;
    }

    void setOperation(int a, int b, int o) {
        operationMatrix[a][b] = o;
    }

    public String addPair(String alignment, char x, char y) {
        if (x == y && y == 0) return alignment;
        String u = "" + x;
        String v = "" + y;
        if (u.equals(v))
            return u + " " + alignment;
        else if (x == 0)
            return "_eps_->" + v + " " + alignment;
        else if (y == 0)
            return u + "->_eps_" + " " + alignment;
        else
            return u + "->" + v + " " + alignment;
    }

    /*
     * Compute alignment between encoded words
     * Synchronized because of nonlocal variables
     */
    public synchronized List<Chunk> alignment(String s, String t) {
        n = s.length();
        m = t.length();

        costMatrix = new double[m + 1][n + 1];
        operationMatrix = new int[m + 1][n + 1];

        int a, b;
        double p1, p2, p3;

        int maxnm = (n > m ? n : m);

        // Fill first row and column of costMatrix

        setCost(0, 0, 0);

        for (a = 1; a <= n; a++) // s horizontal
        {
            setCost(0, a, newCost(getCost(0, a - 1), deleteCost(s.charAt(a - 1)))); // )//   transducer.delta[s.get(a-1)][Alphabet.空]));
            operationMatrix[0][a] = DELETE;
        }

        for (a = 1; a <= m; a++) // t vertical
        {
            setCost(a, 0, newCost(getCost(a - 1, 0), insertCost(t.charAt(a - 1)))); // transducer.delta[Alphabet.空][t.get(a-1)]));
            setOperation(a, 0, INSERT);
        }

        // Fill the rest of the costMatrix
        // Order of filling is in an L-shape: One row, then one column, then one row etc.


        for (a = 1; a <= maxnm; a++) {
            // fill all (remaining) entries in row 'a'. (corresponding to s)

            for (b = a; b <= n; b++) {
                if (a > m) continue;

                p1 = newCost(getCost(a - 1, b - 1), replaceCost(s.charAt(b - 1), t.charAt(a - 1))); // transducer.delta[s.get(b-1)][t.get(a-1)]);
                p2 = newCost(getCost(a - 1, b), insertCost(t.charAt(a - 1))); // transducer.delta[Alphabet.空][t.get(a-1)]);
                p3 = newCost(getCost(a, b - 1), deleteCost(s.charAt(b - 1))); // transducer.delta[s.get(b-1)][Alphabet.空]);

                if (p1 <= p2 && p1 <= p3) {
                    setOperation(a, b, REPLACE);
                } else if (p2 < p3) {
                    setOperation(a, b, INSERT);
                } else {
                    setOperation(a, b, DELETE);
                }
                ;

                setCost(a, b, best(best(p1, p2), p3));
            }

            // fill all (remaining) entries in column 'a'.

            for (b = a; b <= m; b++) {
                if (a > n) continue;

                p1 = newCost(getCost(b - 1, a - 1), replaceCost(s.charAt(a - 1), t.charAt(b - 1))); // transducer.delta[s.get(a-1)][t.get(b-1)]);
                p2 = newCost(getCost(b - 1, a), insertCost(t.charAt(b - 1)));// transducer.delta[Alphabet.空][t.get(b-1)]);
                p3 = newCost(getCost(b, a - 1), deleteCost(s.charAt(a - 1))); // transducer.delta[s.get(a-1)][Alphabet.空]);

                if (p1 <= p2 && p1 <= p3) {
                    setOperation(b, a, REPLACE);
                } else if (p2 < p3) {
                    setOperation(b, a, INSERT);
                } else {
                    setOperation(b, a, DELETE);
                };

                setCost(b, a, best(best(p1, p2), p3));
            }
        }


        //System.out.printf("Cost: %f\n",getCost(m,n););
        //dumpCostMatrix(s,t);
        //System.exit(1);

        b = m;
        a = n;

        int[] changes = new int[2 * maxnm];
        int k = 0;
        String theAlignment = "";

        LinkedList<Chunk> chunks = new LinkedList<>();

        while (a >= 0 && b >= 0 && (a >= 1 || b >= 1)) {
            switch (operationMatrix[b][a]) {
                case REPLACE:
                    changes[k++] = REPLACE;
                    a--;
                    b--;
                    if (a >= 0 && b >= 0)
                    {
                        theAlignment = addPair(theAlignment, s.charAt(a), t.charAt(b));
                        chunks.addFirst(new Chunk(s.charAt(a), t.charAt(b), a )); }
                    break;
                case INSERT:
                    changes[k++] = INSERT;
                    b--;
                    if (b >= 0)
                    {
                        theAlignment = addPair(theAlignment, nul, t.charAt(b));
                        chunks.addFirst(new Chunk(nul, t.charAt(b), a )); }
                    break;
                case DELETE:
                    changes[k++] = DELETE;
                    a--;
                    if (a >= 0)
                    {
                        theAlignment = addPair(theAlignment, s.charAt(a), nul);
                        chunks.addFirst(new Chunk(s.charAt(a), nul, a )); }
                    break;
                default:
                    //fwprintf(stderr,L"ERROR AT %d,%d (%s,%s,%s)\n",a,b,s,t,the_alignment);
                    return clunk(chunks);
            }
        }

        //fprintf(stderr,"k=%d\n",k);

        return chunks;
    }


    public void alignPairsFromFile(java.io.BufferedReader f) {
        String s;
        try {
            while ((s = f.readLine()) != null) {
                String[] t = s.split("\t");
                String x = t[0];
                String y = t[1];
                String z = alignment(x, y).toString();
                System.out.printf("%s\t%s\t%s\n", x, y, z);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static void main(String[] args) {
        Aligner a = new Aligner();
        List<Chunk> x = a.alignment("paaiip", "paaep");
        System.out.println(x + " --> " + clunk(x));
    }
}
