package phpanalysis.analyzer;

import phpanalysis.parser.Trees._;
import phpanalysis.{Main,Reporter};

case class CheckContext(topLevel: Boolean, inCond: Boolean);

case class ASTChecks(node: Tree, context: CheckContext) extends ASTTraversal[CheckContext](node, context) {

    def this(node: Tree) = this(node, CheckContext(true, false))

    /**
     * Visit the nodes and aggregate information inside the context to provide
     * hints about obvious errors directly from the AST
     */
    def visit(node: Tree, ctx: CheckContext): (CheckContext, Boolean) = {
        var newCtx = ctx;
        var continue = true;

        node match {
            case f : FunctionDecl => 
                newCtx = CheckContext(false, false); 
                if (!ctx.topLevel && Main.verbosity >= 2) {
                    Reporter.notice("Function "+f.name.value+" should be declared at top-level", f.name)
                }
            case c: ClassDecl =>
                newCtx = CheckContext(false, false);
                if (!ctx.topLevel && Main.verbosity >= 2) {
                    Reporter.notice("Class "+c.name.value+" should be declared at top-level", c.name)
                }
            case x @ If(cond, then, elze) =>
                // New traversals
                ASTChecks(cond, CheckContext(false, true)).execute
                ASTChecks(then, CheckContext(false, false)).execute
                elze match {
                    case Some(el) =>
                        ASTChecks(el, CheckContext(false, false)).execute
                    case None =>
                }

                continue = false // Do not do twice

            case x @ For(init, cond, step, body) =>
                // New traversals
                ASTChecks(init, CheckContext(false, false)).execute
                ASTChecks(cond, CheckContext(false, true)).execute
                ASTChecks(step, CheckContext(false, false)).execute
                ASTChecks(body, CheckContext(false, false)).execute

                continue = false // Do not do twice
            case x: While =>
                newCtx = CheckContext(false, false)
            case x: DoWhile =>
                newCtx = CheckContext(false, false)
            case x: Foreach =>
                newCtx = CheckContext(false, false)
            case x: Switch =>
                newCtx = CheckContext(false, false)

            // check for call-time pass-by-ref
            case FunctionCall(ref, args) if Main.verbosity >= 2 => {
                for (arg <- args) if (arg.forceref) {
                    Reporter.notice("Usage of call-time pass-by-ref is deprecated and should be avoided", arg)
                }
            }

            case a @ Assign(vr, vl, _) if ctx.inCond && Main.verbosity >= 2 => {
                Reporter.notice("Potential mistake: assignation used in an if condition", node)
            }

            case MethodCall(obj, ref, args) if Main.verbosity >= 2 => {
                for (arg <- args) if (arg.forceref) {
                    Reporter.notice("Usage of call-time pass-by-ref is deprecated and should be avoided", arg)
                }
            }

            case StaticMethodCall(classref, ref, args) if Main.verbosity >= 2 => {
                for (arg <- args) if (arg.forceref) {
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

        (newCtx, continue)
    }

    def execute = traverse(visit)
}
