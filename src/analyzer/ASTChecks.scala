package phpanalysis.analyzer;

import phpanalysis.parser.Trees._;

case class CheckContext(topLevel: Boolean);

case class ASTChecks(node: Tree) extends ASTTraversal[CheckContext](node, CheckContext(true)) with Reporter {

    /**
     * Visit the nodes and aggregate information inside the context to provide
     * hints about obvious errors directly from the AST
     */
    def visit(node: Tree, ctx: CheckContext): CheckContext = {
        var newCtx = ctx;

        node match {
            case f : FunctionDecl => 
                newCtx = CheckContext(false); 
                if (!ctx.topLevel) {
                    notice("Function "+f.name.value+" should be declared at top-level", f.name)
                }
            case c: ClassDecl =>
                newCtx = CheckContext(false);
                if (!ctx.topLevel) {
                    notice("Class "+c.name.value+" should be declared at top-level", c.name)
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

            // check for call-time pass-by-ref
            case FunctionCall(ref, args) => {
                for (val arg <- args) if (arg.forceref) {
                    notice("Usage of call-time pass-by-ref is deprecated and should be avoided", arg)
                }
            }

            case MethodCall(obj, ref, args) => {
                for (val arg <- args) if (arg.forceref) {
                    notice("Usage of call-time pass-by-ref is deprecated and should be avoided", arg)
                }
            }

            case StaticMethodCall(classref, ref, args) => {
                for (val arg <- args) if (arg.forceref) {
                    notice("Usage of call-time pass-by-ref is deprecated and should be avoided", arg)
                }
            }

            case i @ Include(expr, once) => {
                if (!static_expr(expr)) {
                    notice("Include with non trivial argument will be ignored", i)
                }
            }

            case r @ Require(expr, once) => {
                if (!static_expr(expr)) {
                    notice("Require with non trivial argument will be ignored", r)
                }
            }

            case _ =>
        }

        newCtx
    }

    def static_expr(expr: Expression):Boolean = {
        false
    }

}
