package phpanalysis;
import scala.io.Source;
import scala.collection.mutable.HashMap;

/* The reported trait is reponsible to output formatted errors */
trait Reporter {
    private var files = new HashMap[String, List[String]]();

    def error(msg: String, pos: Positional) = {
        emit("Error: ", msg, pos);
    }

    def notice(msg: String, pos: Positional) = {
        emit("Notice: ", msg, pos);
    }

    private def emit(prefix: String, msg: String, pos: Positional) = {
        println(pos.file+":"+pos.line+"  "+prefix+msg)
        print(getFileLine(pos.file, pos.line))

        var indent: String = ""
        for(i <- 0 until pos.col) indent = indent + " ";

        println(indent+"^")

    }


    def getFileLine(file: String, line: Int): String = {
        val l = line-1;

        if (!files.contains(file)) {
            val lines = Source.fromFile(file).getLines.toList
            files += file -> lines
        }
        val lines = files(file)
        if (l >= lines.size || l < 0) {
            scala.Predef.error("Line out of range")
        }
        lines(l)
    }

}
