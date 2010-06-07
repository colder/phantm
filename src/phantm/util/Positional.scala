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

    def getEndPosition = {
        val p = new Position;
        p.file = file;
        p.line = line_end;
        p.col = col_end;
        p
    }

    def setPos(l: Int, c: Int, f: String): self.type = {
        line = l;
        col = c;
        file = Some(f);

        this
    }

    def setEndPos(l: Int, c: Int, f: String): self.type = {
        line_end = l;
        col_end = c;

        this
    }

    def setEndPos(p: Positional): self.type = {
        line_end = p.line_end;
        col_end = p.col_end;
        this
    }

    def setEndPos(p: ParseNode): self.type = {
        // Then, we get the right-most token
        var pRight = p
        var continue = true;
        while (continue && !pRight.isToken) {
            val lst = pRight.children()
            if (lst.size() == 0) {
                continue = false
            } else {
                pRight = lst.get(lst.size()-1);
            }
        }

        line_end = pRight.line
        col_end = pRight.column+pRight.tokenContent.length

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

        // Then, we get the right-most token
        setEndPos(p)
    }

    def setPos(p: Positional): self.type = {
        line = p.line
        col = p.col
        line_end = p.line_end;
        col_end = p.col_end;
        file = p.file
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
