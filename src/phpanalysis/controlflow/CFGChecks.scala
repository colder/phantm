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
            case Program(stmts) =>
                val cfg: CFG = ASTToCFG.convertAST(stmts)
                val tfa = new TypeFlow.Analyzer(cfg)
                tfa.analyze


            case FunctionDecl(name, args, retref, body) =>
                val cfg: CFG = ASTToCFG.convertAST(List(body))
                val tfa = new TypeFlow.Analyzer(cfg)
                tfa.analyze

            case ClassDecl(name, flags, parent, interfaces, methods, static_props, props, consts) =>
                for (m <- methods) if (m.body != None) {
                    val cfg: CFG = ASTToCFG.convertAST(List(m.body.get))
                    val tfa = new TypeFlow.Analyzer(cfg)
                    tfa.analyze
                    cfg
                }

            case _ =>
        }

        (newCtx, true)
    }

    def execute = traverse(visit)
}
