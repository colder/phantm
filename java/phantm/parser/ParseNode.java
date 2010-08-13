package phantm.parser;

import java.util.*;

public final class ParseNode {

    private final int symbol;
    private final String name;

    private List<ParseNode> children = new ArrayList<ParseNode>();

    private int     tokenLine    = -1;
    private int     tokenColumn  = -1;
    private String  tokenContent = "";
    private String  tokenFile    = "";
    private boolean isToken = false;

    private ParseNode parent = null;

    public ParseNode(int symbol, String name) {
        this.symbol = symbol;
        this.name = name;
    }

    public ParseNode(int symbol, String name, String content, int line, int column, String file) {
        this(symbol, name);
        this.tokenContent = content;
        this.tokenLine    = line;
        this.tokenColumn  = column;
        this.isToken      = true;
        this.tokenFile    = file;
    }

    public int symbol() { return symbol; }
    public String name() { return name; }
    public int line() { return tokenLine; }
    public int column() { return tokenColumn; }
    public String file() { return tokenFile; }
    public String tokenContent() { return tokenContent; }
    public boolean isToken() { return isToken; }
    public List<ParseNode> children() { return children; }

    public int[] lineColumnEnd() {
        int[] res = new int[2];
        if (isToken) {
            res[0] = tokenLine;
            res[1] = tokenColumn+tokenContent.length();
            return res;
        } else {
            for (ParseNode child : children) {
                int[] r = child.lineColumnEnd();
                if ((r[0] > res[0]) || (r[0] == res[0] && r[1] > res[1])) {
                    res = r;
                }
            }
            return res;
        }
    }

    public void parentIs(ParseNode node) {
        parent = node;
    }

    public void newChildrenIs(ParseNode node) {
        children.add(node);
    }

    public void print() {
        print("");
    }
    public void print(String indent) {
        System.out.println(indent+ ": "+name+"("+symbol+") " + (isToken ? "(Token)" : ""));
        if (isToken) {
            System.out.println(indent+ " Line: "+tokenLine+" Content: "+tokenContent);
        } else {
            for (ParseNode child : children) {
                child.print(indent+"  ");
            }
        }
    }
 
}


