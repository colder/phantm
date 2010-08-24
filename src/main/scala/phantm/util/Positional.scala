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
        val file_n = p.fileStart()

        if (file_n == null) {
            file = None
        } else {
            file = Some(file_n)
        }

        val resStart = p.lineColumnStart()
        line = resStart(0)
        col  = resStart(1)

        val resEnd   = p.lineColumnEnd()
        line_end = resEnd(0)
        col_end  = resEnd(1)

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
