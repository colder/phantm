package phpanalysis;
import phpanalysis.parser.ParseNode;

trait Positional {
    self =>

    var line: Int = -1;
    var col: Int = -1;
    var file: Option[String] = None;

    def getPos =  file.getOrElse("<unknown>")+" line "+line+" column "+col;

    def setPos(l: Int, c: Int, f: String): self.type = {
        line = l;
        col = c;
        file = Some(f);

        this
    }

    def setPos(p: ParseNode): self.type = {
        var didSet = false;

        def lookUp(p: ParseNode): Unit = {
            if (p.isToken) {
                didSet = true;
                setPos(p.line, p.column, p.file)
            } else {
                val it = p.children().listIterator();
                while (it.hasNext && !didSet) {
                    lookUp(it.next);
                }
            }
        }
        lookUp(p)
        if (!didSet) println("Woops, no position found for "+p.name)
        this
    }

    def setPos(p: Positional): self.type = {
        line = p.line
        col = p.col
        file = p.file
        this
    }
}
