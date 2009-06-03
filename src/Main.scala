package phpanalysis;

import phpanalysis.parser._;
import phpanalysis.analyzer._;
import phpanalysis.parser.Trees.Tree;
import java.io._;

object Main {
    def main(args: Array[String]): Unit = {
        if (args.length > 0) {
            for (file <- args) compile(file)
        } else {
            usage
        }
    }

    def compile(file: String) = {
        try {
            new Compiler(file) compile match {
                case Some(node) => {
                    // Compute the AST FROM the node
                    val t: Tree = new STToAST(node) getAST;
                    Reporter.errorMilestone
                    // Traverse the ast to look for ovious mistakes.
                    ASTChecks(t) execute;
                    Reporter.errorMilestone
                    // Collect symbols and detect obvious types errors
                    CollectSymbols(t) execute;
                    Reporter.errorMilestone

                    // Emit summary of all symbols
                    analyzer.Symbols.emitSummary
                }
                case None => println("Compilation failed.")
            }
        } catch {
            case Reporter.ErrorException(n) => println(n+" error"+(if (n>1) "s" else "")+" occured, abort")
        }
    }

    def usage = {
        println("Usage: phpanalysis <files ...>");
    }
}
