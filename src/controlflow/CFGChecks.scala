package phpanalysis.controlflow;

import phpanalysis.parser.Trees._;
import phpanalysis.analyzer._;

case class CheckContext();

case class CFGChecks(node: Tree) extends ASTTraversal[CheckContext](node, CheckContext()) {

    /**
     * Visit the nodes and aggregate information inside the context to provide
     * hints about obvious errors directly from the AST
     */
    def visit(node: Tree, ctx: CheckContext): (CheckContext, Boolean) = {
        var newCtx = ctx;

        node match {
            case FunctionDecl(name, args, retref, body) =>
                val cfg: CFG = ASTToCFG.convertAST(List(body))

            case _ =>
        }

        (newCtx, true)
    }

    def execute = traverse(visit)
}
