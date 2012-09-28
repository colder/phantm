package phantm.helpers

import phantm.parser.Parser
import phantm.util.{Reporter,IncludeResolver}
import phantm.cfg.ASTToCFG
import phantm.ast.Trees._
import phantm.ast.{ASTTraversal, STToAST}
import phantm.symbols._
import phantm.phases.CollectSymbols

class CFGGraph extends Helper {

    def generate(input: String, printStream: java.io.PrintStream): Unit = {
            val p = new Parser(input)
            p parse match {
                case Some(node) =>
                    val ast = IncludeResolver(STToAST(p, node).getAST).transform;
                    CollectSymbols(ast) execute;

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
                val cfg = ASTToCFG.convertAST(stmts, GlobalSymbols)
                cfg.writeDottyToFile("result.cfg-"+n, "Main");
                n = n + 1;
            case FunctionDecl( name, args, retref, body) =>
                name.getSymbol match {
                    case fs: FunctionSymbol =>
                        val cfg = ASTToCFG.convertAST(List(body), fs)
                        cfg.writeDottyToFile("result.cfg-"+n, name.value);
                        n = n + 1;
                    case _ =>
                        sys.error("Incoherent symbol type, should be function")
                }

            case ClassDecl( name, flags, parent, interfaces, methods, static_props, props, consts) =>
                for (m <- methods) if (m.body != None) {
                    m.name.getSymbol match {
                        case ms: MethodSymbol =>
                            val cfg = ASTToCFG.convertAST(List(m.body.get), ms)
                            cfg.writeDottyToFile("result.cfg-"+n, name.value+"::"+m.name.value);
                            n = n + 1;
                        case _ =>
                            sys.error("Incoherent symbol type, should be Method")
                    }
                }

            case _ =>
        }

        (newCtx, true)
    }

    def execute = traverse(visit)
}
