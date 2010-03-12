package phpanalysis;
import scala.io.Source;
import scala.collection.mutable.{HashMap,HashSet,Set};

/* The reported trait is reponsible to output formatted errors */
object Reporter {
    private var files = new HashMap[String, List[String]]();
    private var errorsCount = 0
    private var noticesCount = 0
    private var totalErrorsCount = 0
    private var totalNoticesCount = 0
    private var errors = new HashMap[Option[String], Set[(String, String, Positional, String)]];

    def error(msg: String) = {
        errorsCount += 1;
        totalErrorsCount += 1;
        println("Error: "+ msg);
    }
    def error(msg: String, pos: Positional) = {
        val error = ("Error: ", msg, pos, pos.getPos);

        if (errors.get(pos.file) == None) {
            errors(pos.file) = HashSet[(String, String, Positional, String)]()
        }

        if (!errors(pos.file).contains(error)) {
            errorsCount += 1;
            errors(pos.file) += error;
        }
    }

    def getNoticesCount = noticesCount
    def getTotalNoticesCount = totalNoticesCount

    def notice(msg: String, pos: Positional) = {
        val notice = ("Notice: ", msg, pos, pos.getPos);

        if (errors.get(pos.file) == None) {
            errors(pos.file) = HashSet[(String, String, Positional, String)]()
        }

        if (!errors(pos.file).contains(notice)) {
            noticesCount += 1;
            totalNoticesCount += 1;
            errors(pos.file) += notice;
        }
    }

    case class ErrorException(en: Int, nn: Int, etn: Int, ntn: Int) extends RuntimeException;

    def errorMilestone = {
        var errorsToDisplay = if (Main.focusOnMainFiles) {
            var errorSet = HashMap[Option[String], Set[(String, String, Positional, String)]]();
            for ((file, errs) <- errors) {
                if ((file != None) && (Main.files contains file.get)) {
                    errorSet(file) = errs
                } else {
                    // decrement error counts
                    for (e <- errs) {
                        if (e._1 == "Error: ") {
                            errorsCount -= 1;
                        } else if (e._1 == "Notice: ") {
                            noticesCount -= 1;
                        }
                    }

                }
            }
            errorSet
        } else {
            errors
        }
        for (errsPerFile <- errorsToDisplay.elements.toList.sort((a,b) => a._1.getOrElse("") < b._1.getOrElse("")).map(x => x._2)) {
            for ((p, msg, pos, _) <- errsPerFile.toList.sort{(x,y) => x._3.line < y._3.line || (x._3.line == y._3.line && x._3.col < y._3.col)}) {
                emit(p, msg, pos)
            }
            println
        }
        errors.clear
        if (errorsCount > 0) {
            val ec = errorsCount;
            val tec = totalErrorsCount;
            errorsCount = 0;
            totalErrorsCount = 0;
            throw new ErrorException(ec, noticesCount, tec, totalNoticesCount);
        }
    }

    private def emit(prefix: String, msg: String, pos: Positional) = {
        println(pos.file.getOrElse("?")+":"+(if (pos.line < 0) "?" else pos.line)+"  "+prefix+msg)
        pos.file match {
            case Some(file) =>
                getFileLine(file, pos.line) match {
                    case Some(s) =>
                        println(s)

                        var indent: String = ""
                        for(i <- 0 until pos.col) indent = indent + " ";

                        val size = if (pos.line == pos.line_end) {
                            if (pos.col_end > s.length) {
                                s.length - pos.col
                            } else {
                                pos.col_end - pos.col
                            }
                        } else if (pos.line < pos.line_end) {
                            s.length - pos.col
                        } else {
                            1
                        }

                        if (size == 1) {
                          println(indent+Console.RED+"^"+Console.RESET)
                        } else {
                          println(indent+Console.RED+(1 to size).map(i => "~").mkString+Console.RESET)
                        }
                    case None =>
                }
            case None =>
        }

    }


    def getFileLine(file: String, line: Int): Option[String] = {
        val l = line-1;

        try {
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
            Some(lines(l))
        } catch {
            case _ =>
                None
        }
    }

}
