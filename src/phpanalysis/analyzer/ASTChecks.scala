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
                if (!ctx.topLevel) {
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
                for (val arg <- args) if (arg.forceref) {
                    Reporter.notice("Usage of call-time pass-by-ref is deprecated and should be avoided", arg)
                }
            }

            case MethodCall(obj, ref, args) => {
                for (val arg <- args) if (arg.forceref) {
                    Reporter.notice("Usage of call-time pass-by-ref is deprecated and should be avoided", arg)
                }
            }

            case StaticMethodCall(classref, ref, args) => {
                for (val arg <- args) if (arg.forceref) {
                    Reporter.notice("Usage of call-time pass-by-ref is deprecated and should be avoided", arg)
                }
            }

            case i @ Include(expr, once) => {
                if (!static_expr(expr)) {
                    // Todo: Flag the include as ignored
                    Reporter.notice("Include with non trivial argument will be ignored", i)
                }
            }

            case r @ Require(expr, once) => {
                if (!static_expr(expr)) {
                    // Todo: Flag the require as ignored
                    Reporter.notice("Require with non trivial argument will be ignored", r)
                }
            }

            // Check for variable variables
            case v @ VariableVariable(ex) => {
                Reporter.notice("Variable variables should be avoided, use arrays instead", v)
            }

            case d @ DynamicObjectProperty(o, ex) => {
                Reporter.notice("Dynamic object properties should be avoided", ex)
            }

            case _ =>
        }

        (newCtx, true)
    }

    def static_expr(expr: Expression):Boolean = {
        expr match {
            case Concat (lhs, rhs) => 
                static_expr(lhs) && static_expr(rhs)
            case FunctionCall(StaticFunctionRef(_,_,Identifier("dirname")), _) =>
                true
            case Constant(_) =>
                true
            case ClassConstant(_:StaticClassRef, _) =>
                true
            case _: Scalar =>
                true
            case _ => false;
        }
    }

    def execute = traverse(visit)
}
