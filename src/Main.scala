package phpanalysis;

import phpanalysis.parser._;
import java.io._;

object Main {
    def main(args: Array[String]) {
         var l: Lexer = null;

         try {
            l = new Lexer(new java.io.FileReader(args(0)));
         } catch {
            case e: Exception => println("Error: " + e.getMessage)
         }
    }
}
