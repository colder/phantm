package phpanalysis;

import phpanalysis.parser._;
import scala.collection.mutable.HashMap;
import java.io._;

class Compiler(filename: String) {
    type LexerComment = phpanalysis.parser.Lexer#Comment;
    case class Position(_line: Int, _col: Int, _file: String) extends Positional {
        col  = _col;
        line = _line;
        file = Some(_file);
    }

    private var comments = List[(Positional, String)]();

    def clearPreviousComment(pos: Positional): Option[String] =
        getPreviousComment(pos)

    def getPreviousComment(pos: Positional): Option[String] = {
        var comm: Option[String] = None
        var continue = true;

        while(continue) {
            if (comments == Nil || pos < comments.head._1) {
                continue = false
            } else {
                comm = Some(comments.head._2)
                comments = comments.tail
            }
        }

        comm
    }

    def compile: Option[ParseNode] = {
         var l: Lexer = null;

         try {
            l = new Lexer(new java.io.FileReader(filename));
            l.setFileName(filename);
            val p = new Parser(l);
            val r: ParseNode = p.parse().value.asInstanceOf[ParseNode];

            for (c <- JavaListIteratorWrapper[LexerComment](l.comments.iterator)) {
                comments = (Position(c.line, c.col, c.filename), c.content) :: comments;
            }

            comments = comments.reverse
            Some(r)

         } catch {
            case e: FileNotFoundException => error("File not found: "+filename); None
            case e: IOException => error("IOException: " + e.getMessage); None
            case e: Exception => error("Parsing failed"); None
         }
    }

    def error(msg: String) {
        println("Error: "+msg);
    }
}
