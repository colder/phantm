package phpanalysis;

import phpanalysis.parser._;
import java.io._;

class Compiler(filename: String) {

    def compile: Option[ParseNode] = {
         var l: Lexer = null;

         try {
            l = new Lexer(new java.io.FileReader(filename));
            l.setFileName(filename);
            val p = new Parser(l);
            val r: ParseNode = p.parse().value.asInstanceOf[ParseNode];

            Some(r)

         } catch {
            case e: FileNotFoundException => error("File not found: "+filename); None
            case e: IOException => error("IOException: " + e.getMessage); None
         }
    }

    def error(msg: String) {
        println("Error: "+msg);
    }
}
