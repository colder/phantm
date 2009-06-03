package phpanalysis;
import scala.io.Source;
import scala.collection.mutable.HashMap;

/* The reported trait is reponsible to output formatted errors */
object Reporter {
    private var files = new HashMap[String, List[String]]();
    private var errorsCount = 0
    private var noticesCount = 0

    def error(msg: String) = {
        errorsCount += 1;
        println("Error: "+ msg);
    }
    def error(msg: String, pos: Positional) = {
        errorsCount += 1;
        emit("Error: ", msg, pos);
    }

    def notice(msg: String, pos: Positional) = {
        noticesCount += 1;
        emit("Notice: ", msg, pos);
    }

    case class ErrorException(n: Int) extends RuntimeException;

    def errorMilestone = {
        if (errorsCount > 0)
            throw new ErrorException(errorsCount);
        else
            errorsCount = 0;
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
