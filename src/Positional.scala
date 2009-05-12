package phpanalysis;
import phpanalysis.parser.ParseNode;

trait Positional {
    self =>

    var line: Int = -1;
    var col: Int = -1;
    var file: String = "<Unknown>";

    def setPos(l: Int, c: Int, f: String): self.type = {
        line = l;
        col = c;
        file = f;

        this
    }

    def setPos(p: ParseNode): self.type = {
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
        this
    }

    def setPos(p: Positional): self.type = {
        line = p.line
        col = p.col
        file = p.file
        this
    }
}
