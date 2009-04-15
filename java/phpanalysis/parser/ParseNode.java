package phpanalysis.parser;

import java.util.*;

public final class ParseNode {

    private final int symbol;
    private final String name;

    private List<ParseNode> children = new ArrayList<ParseNode>();

    private int     tokenLine = -1;
    private String  tokenContent = "";
    private boolean isToken = false;

    private ParseNode parent = null;

    public ParseNode(int symbol, String name) {
        this.symbol = symbol;
        this.name = name;
    }

    public ParseNode(int symbol, String name, String content, int line) {
        this(symbol, name);
        this.tokenContent = content;
        this.tokenLine    = line;
        this.isToken      = true;
    }

    public int symbol() { return symbol; }
    public String name() { return name; }
    public int tokenLine() { return tokenLine; }
    public String tokenContent() { return tokenContent; }
    public boolean isToken() { return isToken; }
    public List<ParseNode> children() { return children; }

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


