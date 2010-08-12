package phantm.util;
import phantm.parser.ParseNode;

class Position extends Positional {
    override def toString = getPos
}

trait Positional {
    self =>

    var line: Int = -1;
    var col: Int = -1;
    var length: Int = 1;
    var file: Option[String] = None;

    def < (p: Positional) = {
        line < p.line || (line == p.line && col < p.col)
    }

    def getPos = file.getOrElse("<unknown>")+" line "+line+" column "+col;

    def setPos(line: Int, col: Int, length: Int, file: String): self.type = {
        this.line   = line;
        this.col    = col;
        this.length = length;
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
        length = p.length()

        this
    }

    def setPosBetween(from: Positional, to: Positional): self.type = {
        setPos(from)
        if (from.line != to.line) {
            length = -1;
        } else {
            length = to.col-from.col;
        }
        this
    }

    def setPos(p: Positional): self.type = {
        line   = p.line
        col    = p.col
        length = p.length
        file   = p.file
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
