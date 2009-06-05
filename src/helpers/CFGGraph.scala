package phpanalysis.helpers;

import phpanalysis.controlflow._;
import phpanalysis.analyzer._;
import phpanalysis.parser.Trees._;
import phpanalysis.parser.STToAST;

class CFGGraph extends Helper {

    def generate(input: String, printStream: java.io.PrintStream): Unit = {
            new Compiler(input) compile match {
                case Some(node) =>
                    val ast = STToAST(node).getAST;
                    
                    CollectSymbols(ast) execute;
                    Reporter.errorMilestone

                    CFGGraphs(ast).execute
                case None =>
                    throw new Exception("Compilation failed");

            }
    }

}

case class CheckContext();

case class CFGGraphs(node: Tree) extends ASTTraversal[CheckContext](node, CheckContext()) {

    /**
     * Visit the nodes and aggregate information inside the context to provide
     * hints about obvious errors directly from the AST
     */
    def visit(node: Tree, ctx: CheckContext): (CheckContext, Boolean) = {
        var newCtx = ctx;

        node match {
            case FunctionDecl(name, args, retref, body) =>
                val cfg: CFG = ASTToCFG.convertAST(List(body))
                cfg.writeDottyToFile("result.cfg")

            case _ =>
        }

        (newCtx, true)
    }

    def execute = traverse(visit)
}
