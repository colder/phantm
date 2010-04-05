package phantm.helpers;

import phantm.parser._;
import java.io._;

class Lint extends Helper{

    def generate(input: String, printStream: java.io.PrintStream): Unit = {
        try {
            val l = new Lexer(new java.io.FileReader(input));
            l.setFileName(input);
            val p = new Parser(l);
            p.parse();
            printStream.println("No syntax errors detected in "+input);
        } catch {
            case e: FileNotFoundException =>
                error("File not found: "+input);
            case e: IOException =>
                error("IOException: " + e.getMessage);
            case e: Exception =>
        }

    }
}
