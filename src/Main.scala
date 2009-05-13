package phpanalysis;

import phpanalysis.parser._;
import phpanalysis.analyzer._;
import phpanalysis.parser.Trees.Tree;
import java.io._;

object Main {
    def main(args: Array[String]): Unit = {

        if (args.length > 0) {
            for (file <- args) {
                new Compiler(file) compile match {
                    case Some(node) => {
                        val t: Tree = new STToAST(node) getAST;
                        // Traverse the ast to look for ovious mistakes.
                        ASTChecks(t) traverse;
                        CollectSymbols(t) traverse;
                        analyzer.Symbols.emitSummary
                    }
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
