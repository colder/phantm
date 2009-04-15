package phpanalysis;

import phpanalysis.parser._;
import java.io._;

class Compiler(filename: String) {

    def compile: Unit = {
         var l: Lexer = null;

         try {
            println("Compiling "+filename+" ");
            l = new Lexer(new java.io.FileReader(filename));
            l.setFileName(filename);
            val p = new Parser(l);
            val r: ParseNode = p.parse().value.asInstanceOf[ParseNode];

            r print

         } catch {
            case e: FileNotFoundException => error("File not found: "+filename)
            case e: IOException => error("IOException: " + e.getMessage)
         }
    }

    def error(msg: String) {
        println("Error: "+msg);
    }
}
