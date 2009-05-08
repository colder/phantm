package phpanalysis;
import phpanalysis.parser.ParseNode;

trait Positional {
    var line: Int = -1;
    var col: Int = -1;
    var file: String = "<Unknown>";

    def setPos(l: Int, c: Int, f: String): Unit = {
        line = l;
        col = c;
        file = f;
    }

    def setPos(p: ParseNode): Unit = {
        def lookUp(p: ParseNode): Unit = {
            if (p.isToken) {
                setPos(p.line, p.column, p.file)
            } else {
                val it = p.children().listIterator();
                if (it.hasNext) {
                    lookUp(it.next);
                }
            }
        }
        lookUp(p)

    }

    def setPos(p: Positional): Unit = {
        line = p.line
        col = p.col
        file = p.file
    }
}
