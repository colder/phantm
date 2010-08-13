package phantm.util;
import phantm.parser.ParseNode;

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

    def setPos(line: Int, col: Int, file: String): self.type = {
        this.line   = line;
        this.col    = col;
        this.file = Some(file);
        this
    }

    def setPos(p: ParseNode): self.type = {
        import Math.max

        // First, we get the left-most token
        var pLeft = p
        var continue = true;
        while (continue && !pLeft.isToken) {
            val lst = pLeft.children()
            if (lst.size() == 0) {
                continue = false
            } else {
                pLeft = pLeft.children().get(0);
            }
        }
        line = pLeft.line
        col = pLeft.column
        file = Some(pLeft.file)

        // Then, we calculate the length of the parsenode
        val res = p.lineColumnEnd()

        line_end = res(0)
        col_end  = res(1)

        this
    }

    def setPosBetween(from: Positional, to: Positional): self.type = {
        setPos(from)
        line_end = to.line_end
        col_end  = to.col_end
        this
    }

    def setPos(p: Positional): self.type = {
        line     = p.line
        col      = p.col
        line_end = p.line_end
        col_end  = p.col_end
        file     = p.file
        this
    }

    def previousPos = {
        if (file == None) {
            "internally"
        } else {
            "in "+getPos
        }
    }
}
