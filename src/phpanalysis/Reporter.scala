package phpanalysis;
import scala.io.Source;
import scala.collection.mutable.{HashMap,HashSet};

/* The reported trait is reponsible to output formatted errors */
object Reporter {
    private var files = new HashMap[String, List[String]]();
    private var errorsCount = 0
    private var noticesCount = 0
    private var errors = new HashSet[(String, String, Positional, String)]();

    def error(msg: String) = {
        errorsCount += 1;
        println("Error: "+ msg);
    }
    def error(msg: String, pos: Positional) = {
        errorsCount += 1;
        errors += (("Error: ", msg, pos, pos.getPos));
    }

    def notice(msg: String, pos: Positional) = {
        noticesCount += 1;
        errors += (("Notice: ", msg, pos, pos.getPos));
    }

    case class ErrorException(n: Int) extends RuntimeException;

    def errorMilestone = {
        for ((p, msg, pos, _) <- errors.toList.sort{(x,y) => x._3.line < y._3.line || (x._3.line == y._3.line && x._3.col < y._3.col)}) {
            emit(p, msg, pos)
        }
        if (errorsCount > 0) {
            val ec = errorsCount;
            errorsCount = 0;
            throw new ErrorException(ec);
        }
    }

    private def emit(prefix: String, msg: String, pos: Positional) = {
        println(pos.file+":"+pos.line+"  "+prefix+msg)
        println(getFileLine(pos.file, pos.line))

        var indent: String = ""
        for(i <- 0 until pos.col) indent = indent + " ";

        println(indent+"^")

    }


    def getFileLine(file: String, line: Int): String = {
        val l = line-1;

        if (!files.contains(file)) {
            import java.io.{BufferedReader, FileReader}
            val input =  new BufferedReader(new FileReader(file));
            var line = "";
            var lines: List[String] = Nil;
            line = input.readLine()
            while (line != null){
                lines = line.replaceAll("\t", " ").replaceAll("\r", "") :: lines;
                line = input.readLine()
            }

            files += file -> lines.reverse
        }
        val lines = files(file)
        if (l >= lines.size || l < 0) {
            scala.Predef.error("Line out of range")
        }
        lines(l)
    }

}
