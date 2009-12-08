package phpanalysis.helpers;

import phpanalysis.controlflow._;
import phpanalysis.analyzer._;
import phpanalysis.parser.Trees._;
import phpanalysis.parser.STToAST;

class CFGGraph extends Helper {

    def generate(input: String, printStream: java.io.PrintStream): Unit = {
            new Compiler(input) compile match {
                case Some(node) =>
                    val ast = IncludeResolver(STToAST(node).getAST).transform;
                    
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
    var result: String = "";
    var n = 1;

    /**
     * Visit the nodes and aggregate information inside the context to provide
     * hints about obvious errors directly from the AST
     */
    def visit(node: Tree, ctx: CheckContext): (CheckContext, Boolean) = {
        var newCtx = ctx;

        node match {
            case Program(stmts) =>
                val cfg: CFG = ASTToCFG.convertAST(stmts)
                cfg.writeDottyToFile("result.cfg-"+n, "Main");
                n = n + 1;
            case FunctionDecl(name, args, retref, body) =>
                val cfg: CFG = ASTToCFG.convertAST(List(body))
                cfg.writeDottyToFile("result.cfg-"+n, name.value);
                n = n + 1;

            case ClassDecl(name, flags, parent, interfaces, methods, static_props, props, consts) =>
                for (m <- methods) if (m.body != None) {
                    val cfg: CFG = ASTToCFG.convertAST(List(m.body.get))
                    cfg.writeDottyToFile("result.cfg-"+n, name.value+"::"+m.name.value);
                    n = n + 1;
                }

            case _ =>
        }

        (newCtx, true)
    }

    def execute = traverse(visit)
}
