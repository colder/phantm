package phpanalysis;

import phpanalysis.parser._;
import java.io._;

object Main {
    def main(args: Array[String]): Unit = {

        if (args.length > 0) {
            for (file <- args) {
                new Compiler(file) compile match {
                    case Some(node) => node print
                    case None => println("Compilation failed.")
                }
            }
        } else {
            usage
        }
    }

    def usage = {
        println("Usage: phpanalysis <files ...>");
    }
}
