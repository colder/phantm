package phpanalysis;

import phpanalysis.parser._;
import phpanalysis.analyzer._;
import phpanalysis.controlflow._;
import phpanalysis.parser.Trees.Program;
import java.io._;

object Main {
    var files: List[String] = Nil;
    var displaySymbols = false;
    var displayDebug   = false;

    def main(args: Array[String]): Unit = {
        if (args.length > 0) {
            handleArgs(args.toList);

            if (files.length == 0) {
                println("No file provided.")
                usage
            } else {
                for (file <- files) compile(file)
            }
        } else {
            usage
        }
    }

    def handleArgs(args: List[String]): Unit = args match {
        case "--symbols" :: xs => displaySymbols = true; handleArgs(xs);
        case "--debug" :: xs => displayDebug = true; handleArgs(xs);
        case x :: xs => files = files ::: x :: Nil; handleArgs(xs);
        case Nil => 
    }

    def compile(file: String) = {
        try {
            new Compiler(file) compile match {
                case Some(node) => {
                    // Compute the AST FROM the node
                    var ast: Program = new STToAST(node) getAST;
                    Reporter.errorMilestone
                    // Run AST transformers
                    ast = IncludeResolver(ast).transform
                    // Traverse the ast to look for ovious mistakes.
                    ASTChecks(ast) execute;
                    Reporter.errorMilestone
                    // Collect symbols and detect obvious types errors
                    CollectSymbols(ast) execute;
                    Reporter.errorMilestone

                    if (displaySymbols) {
                        // Emit summary of all symbols
                        analyzer.Symbols.emitSummary
                    }

                    // Build CFGs and analyzes them
                    CFGChecks(ast) execute;
                    Reporter.errorMilestone

                }
                case None => println("Compilation failed.")
            }
        } catch {
            case Reporter.ErrorException(n) => println(n+" error"+(if (n>1) "s" else "")+" occured, abort")
        }
    }

    def usage = {
        println("Usage:   phpanalysis [-s] <files ...>");
        println("Options: -s : Print symbol summary");
    }
}
