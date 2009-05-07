package phpanalysis.analyzer;

import phpanalysis.parser.Trees._;

case class CheckContext(topLevel: Boolean);

case class ASTChecks(node: Tree) extends ASTTraversal[CheckContext](node, CheckContext(true)) with Reporter {

    def visit(node: Tree, ctx: CheckContext): CheckContext = {
        var newCtx = ctx;

        node match {
            case f : FunctionDecl => 
                newCtx = CheckContext(false); 
                if (!ctx.topLevel) {
                    error("Function "+f.name.value+" should be declared at top-level", f.name)
                }
            case c: ClassDecl =>
                newCtx = CheckContext(false);
                if (!ctx.topLevel) {
                    error("Class "+c.name.value+" should be declared at top-level", c.name)
                }
            case x: If =>
                newCtx = CheckContext(false)
            case x: While =>
                newCtx = CheckContext(false)
            case x: DoWhile =>
                newCtx = CheckContext(false)
            case x: Foreach =>
                newCtx = CheckContext(false)
            case x: Switch =>
                newCtx = CheckContext(false)
            case _ =>
        }

        newCtx
    }

}
