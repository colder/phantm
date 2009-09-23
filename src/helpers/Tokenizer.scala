package phpanalysis.helpers;

import phpanalysis.parser._;
import java.io._;

class Tokenizer extends Helper{

    def generate(input: String, printStream: java.io.PrintStream): Unit = {
        try {
               val l = new Lexer(new java.io.FileReader(input));
               l.setFileName(input);
               var sym: java_cup.runtime.Symbol = l.next_token();

               while(sym != null && sym.sym != 0) {
                    val pn: ParseNode = sym.value.asInstanceOf[ParseNode];
                    if (pn != null) {
                        printStream.println(pn.name + "("+pn.tokenContent+")");
                        //printStream.println(pn.name);
                        sym = l.next_token();
                    } else {
                        printStream.println("Unknown("+sym.sym+")");
                    }
               }

            } catch {
               case e: FileNotFoundException => error("File not found: "+input); None
               case e: IOException => error("IOException: " + e.getMessage); None
            }

    }
}
