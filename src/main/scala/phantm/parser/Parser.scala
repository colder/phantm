package phantm.parser

import java.io._
import phantm.util._
import phantm.annotations.SourceAnnotations.{Parser => AnnotationsParser}

class Parser(filename: String) {
    type LexerComment = phantm.parser.Comment;

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

    def parse: Option[ParseNode] = {
         var l: Lexer = null;

         try {
            l = new Lexer(new java.io.FileReader(filename));
            l.setFileName(filename);
            val p = new CUPParser(l);
            val r: ParseNode = p.parse().value.asInstanceOf[ParseNode];

            for (c <- JavaListIteratorWrapper[LexerComment](l.comments.iterator)) {
                comments = (new Position().setPos(c.line, c.col, c.filename), c.content) :: comments;
            }

            comments = comments.reverse

            // We import and define typedefs
            for (c <- comments) {
                AnnotationsParser.importTypeDefs(c._2)
            }

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
