package phpanalysis.analyzer;

import phpanalysis.parser.Trees._;

case class CheckContext(topLevel: Boolean);

case class ASTChecks(node: Tree) extends ASTTraversal[CheckContext](node, CheckContext(true)) {

    /**
     * Visit the nodes and aggregate information inside the context to provide
     * hints about obvious errors directly from the AST
     */
    def visit(node: Tree, ctx: CheckContext): (CheckContext, Boolean) = {
        var newCtx = ctx;

        node match {
            case f : FunctionDecl => 
                newCtx = CheckContext(false); 
                if (!ctx.topLevel) {
                    Reporter.notice("Function "+f.name.value+" should be declared at top-level", f.name)
                }
            case c: ClassDecl =>
                newCtx = CheckContext(false);
                if (!ctx.topLevel && Main.verbosity >= 2) {
                    Reporter.notice("Class "+c.name.value+" should be declared at top-level", c.name)
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
                for (val arg <- args) if (arg.forceref && Main.verbosity >= 2) {
                    Reporter.notice("Usage of call-time pass-by-ref is deprecated and should be avoided", arg)
                }
            }

            case MethodCall(obj, ref, args) => {
                for (val arg <- args) if (arg.forceref && Main.verbosity >= 2) {
                    Reporter.notice("Usage of call-time pass-by-ref is deprecated and should be avoided", arg)
                }
            }

            case StaticMethodCall(classref, ref, args) => {
                for (val arg <- args) if (arg.forceref && Main.verbosity >= 2) {
                    Reporter.notice("Usage of call-time pass-by-ref is deprecated and should be avoided", arg)
                }
            }

            // Check for variable variables
            case v @ VariableVariable(ex) if Main.verbosity >= 3 => {
                Reporter.notice("Variable variables should be avoided, use arrays instead", v)
            }

            case d @ DynamicObjectProperty(o, ex) if Main.verbosity >= 3 => {
                Reporter.notice("Dynamic object properties should be avoided", ex)
            }

            case _ =>
        }

        (newCtx, true)
    }

    def execute = traverse(visit)
}
