package phpanalysis;

import phpanalysis.parser._;
import java.io._;

class Compiler(filename: String) {

    def compile: Unit = {
         var l: Lexer = null;

         try {
            l = new Lexer(new java.io.FileReader(filename));
            l.setFileName(filename);
            val p = new Parser(l);
            val r = p.parse().value;
         } catch {
            case e: FileNotFoundException => error("File not found: "+filename)
            case e: IOException => error("IOException: " + e.getMessage)
         }
    }

    def error(msg: String) {
        println("Error: "+msg);
    }
}
