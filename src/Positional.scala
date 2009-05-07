package phpanalysis;

trait Positional {
    var line: Int = -1;
    var col: Int = -1;
    var file: String = "<Unknown>";

    def setPos(l: Int, c: Int, f: String) = {
        line = l;
        col = c;
        file = f;
    }

    def setPos(p: Positional) {
        line = p.line
        col = p.col
        file = p.file
    }
}
