package phpanalysis;
import phpanalysis.parser.ParseNode;

class Position extends Positional {
    override def toString = getPos
}

trait Positional {
    self =>

    var line: Int = -1;
    var col: Int = -1;
    var line_end: Int = -1;
    var col_end: Int = -1;
    var file: Option[String] = None;

    def < (p: Positional) = {
        line < p.line || (line == p.line && col < p.col)
    }

    def getPos = file.getOrElse("<unknown>")+" line "+line+" column "+col;

    def setPos(l: Int, c: Int, f: String): self.type = {
        line = l;
        col = c;
        file = Some(f);

        this
    }

    def setPos(p: ParseNode): self.type = {
        import Math.max

        var didSet = false;
        var l_end = -1;
        var c_end = -1;

        def lookUp(p: ParseNode): Unit = {
            if (p.isToken) {
                if (!didSet) {
                    setPos(p.line, p.column, p.file)
                    didSet = true
                }

                l_end = max(p.line, l_end);
                c_end = max(p.column+p.tokenContent.length, c_end);
            } else {
                val it = p.children().listIterator();
                while (it.hasNext) {
                    lookUp(it.next);
                }
            }
        }
        lookUp(p)

        line_end = l_end;
        col_end  = c_end;

        if (!didSet) println("Woops, no position found for "+p.name)
        this
    }

    def setPos(p: Positional): self.type = {
        line = p.line
        col = p.col
        line_end = p.line_end;
        col_end = p.col_end;
        file = p.file
        this
    }
}
