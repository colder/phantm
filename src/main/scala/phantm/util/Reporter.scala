package phantm.util
import scala.io.Source
import phantm.Settings

sealed abstract class ErrorTag
case object ENotice extends ErrorTag

class Reporter(mainFiles: List[String]) {

    case class Error(val message: String, val pos: Positional, var tags: Set[ErrorTag]) {
        override def equals(o: Any): Boolean = o match {
            case e2: Error =>
                if (Settings.get.compactErrors) {
                    // At most one error per line
                    (tags == e2.tags) && (pos.line == e2.pos.line)
                } else {
                    (tags == e2.tags) && (pos.getPos == e2.pos.getPos) && message == e2.message
                }
            case _ =>
                false
        }

        override def hashCode =
            if (Settings.get.compactErrors) {
                // At most one error per line
                tags.hashCode+pos.line.hashCode
            } else {
                tags.hashCode+pos.getPos.hashCode+message.hashCode
            }

        def before(e2: Error) = {
            pos.line < e2.pos.line || (pos.line == e2.pos.line && pos.col < e2.pos.col)
        }
    }

    var tickCount = 0

    def beginTicks = {
        tickCount = 0
    }

    def tick = {
        tickCount += 1
        print(".")
        if (tickCount == 80) {
            println
            tickCount = 0
        }
    }

    def endTicks = {
        if (tickCount != 0) {
            println
            tickCount = 0
        }
    }

    private var files = Map[String, List[String]]();
    private var errors = Map[Option[String], Set[Error]]().withDefaultValue(Set[Error]())

    def error(msg: String) = {
        println("Error: "+ msg);
        true
    }

    def error(msg: String, pos: Positional, tags: Set[ErrorTag] = Set()) = addError(Error(msg, pos, tags))

    def notice(msg: String) = {
        println("Notice: "+ msg);
        true
    }

    def notice(msg: String, pos: Positional, tags: Set[ErrorTag] = Set()) = addError(Error(msg, pos, tags + ENotice))

    def addError(e: Error): Boolean = {
        if (!errors(e.pos.file).contains(e)) {
            errors += (e.pos.file -> (errors(e.pos.file) + e))
            true
        } else {
            false
        }
    }


    def emitAll = {
        for (errsPerFile <- errors.iterator.toList.sortWith((a,b) => a._1.getOrElse("") < b._1.getOrElse("")).map(x => x._2)) {
            for (e <- errsPerFile.toList.sortWith{(x, y) => x before y}) {
                emit(e.tags, e.message, e.pos)
            }
        }
    }

    def clear = {
        errors = errors.empty
    }

    def clearTag(tag: ErrorTag) = {
        for ((f,ers) <- errors) {
            errors += (f -> ers.filter(e => !(e.tags contains tag)))
        }
    }

    private def emitNormal(tags: Set[ErrorTag], msg: String, pos: Positional) = {
        val kind = if(tags contains ENotice) "Notice: " else "Error: ";

        println(pos.file.getOrElse("?")+":"+(if (pos.line < 0) "?" else pos.line)+"  "+kind+msg)
        pos.file match {
            case Some(file) =>
                getFileLine(file, pos.line) match {
                    case Some(s) =>

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


                        if (Settings.get.format != "termbg") {
                            println(s)

                            val (colorBegin, colorEnd) = if (Settings.get.format == "term") {
                                (Console.RED+Console.BOLD, Console.RESET)
                            } else if (Settings.get.format == "html") {
                                ("<span style=\"color: red;\">", "</span>")
                            } else {
                                ("", "")
                            }

                            if (size == 1) {
                              println(indent+colorBegin+"^"+colorEnd)
                            } else {
                              println(indent+colorBegin+(1 to size).map(i => "~").mkString+colorEnd)
                            }
                        } else {
                            print(s.substring(0, pos.col))
                            print(Console.RED_B)
                            print(s.substring(pos.col, pos.col+size))
                            print(Console.RESET)
                            println(s.substring(pos.col+size))
                            println
                        }

                    case None =>
                }
            case None =>
        }
    }

    private def emitQuickFix(tags: Set[ErrorTag], msg: String, pos: Positional) = {
        val kind = if(tags contains ENotice) "Notice: " else "Error: "
        println(pos.file.getOrElse("?")+":"+(if (pos.line < 0) "?" else pos.line)+":"+(if (pos.col < 0) "?" else pos.col+1)+"  "+kind+msg)
    }

    private def emit(tags: Set[ErrorTag], msg: String, pos: Positional) = {
        if (Settings.get.format == "quickfix") {
            emitQuickFix(tags, msg, pos)
        } else {
            emitNormal(tags, msg, pos)
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

                files += (file -> lines.reverse)
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

    def emitSummary = {

        val (notices, errs) = errors.values.foldRight(Set[Error]())(_ ++ _).partition(e => e.tags contains ENotice)
        val noticeCount = notices.size
        val errorCount  = errs.size

        println(noticeCount+" notice"+(if (noticeCount>1) "s" else "")+" and "+errorCount+" error"+(if (errorCount>1) "s" else "")+" occured.")
    }

}

object Reporter {
    private var rep: Option[Reporter] = None

    def get = rep.getOrElse(throw new RuntimeException("Undefined Reporter instance"))

    def set(newrep: Reporter) = this.rep = Some(newrep)

    def error(msg: String) =
        get.error(msg)

    def error(msg: String, pos: Positional, tags: Set[ErrorTag] = Set()) =
        get.error(msg, pos, tags)

    def notice(msg: String) =
        get.notice(msg)

    def notice(msg: String, pos: Positional, tags: Set[ErrorTag] = Set()) =
        get.notice(msg, pos, tags)
}

case class ErrorException(en: Int, nn: Int, etn: Int, ntn: Int) extends RuntimeException;
