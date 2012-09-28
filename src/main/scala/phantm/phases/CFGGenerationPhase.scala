package phantm.phases;

import phantm.Settings
import phantm.cfg.{ASTToCFG}
import phantm.ast.Trees._
import phantm.cfg.{Trees => CFG}
import phantm.ast.ASTSimpleTraversal
import phantm.util.Reporter
import phantm.symbols._

object CFGGenerationPhase extends Phase {

    def name = "CFGs generation"
    def description = "Generating CFGs"

    def run(ctx: PhasesContext): PhasesContext = {
        // Print summary header, if requested
        val cfgg = CFGGenerator(ctx, ctx.oast.get);

        cfgg.execute

        // Return the context populated with CFGs
        cfgg.ctx
    }

}


case class CFGGenerator(initCtx: PhasesContext, node: Tree) extends ASTSimpleTraversal(node) {
    var ctx = initCtx

    def display(content: String) = {
        if (Settings.get.displayProgress && Settings.get.verbosity > 2) {
            println("     - "+content)
        }
    }

    def visit(node: Tree): Boolean = {
        node match {
            case Program(stmts) =>
                display("Converting main scope...")
                val cfg = ASTToCFG.convertAST(stmts, ctx.globalSymbols, ctx)

                ctx = ctx.copy(cfgs = ctx.cfgs + (None -> cfg))

            case FunctionDecl(name, args, retref, body) =>
                name.getSymbol match {
                    case fs: FunctionSymbol =>
                        display("Converting function "+name.value+"...")
                        val cfg = ASTToCFG.convertAST(List(body), fs, ctx)

                        ctx = ctx.copy(cfgs = ctx.cfgs + (Some(fs) -> cfg))
                    case _ =>
                        sys.error("Incoherent symbol type, should be function")
                }


            case ClassDecl(name, flags, parent, interfaces, methods, static_props, props, consts) =>
                name.getSymbol match {
                    case cl: ClassSymbol =>
                        for (m <- methods) if (m.body != None) {
                            m.name.getSymbol match {
                                case ms: MethodSymbol =>
                                    display("Converting method "+cl.name+"::"+m.name.value+"...")
                                    val cfg = ASTToCFG.convertAST(List(m.body.get), ms, ctx)
                                    ctx = ctx.copy(cfgs = ctx.cfgs + (Some(ms) -> cfg))
                                case _ =>
                                    sys.error("Incoherent symbol type, should be method")
                            }
                        }
                    case _ =>
                        sys.error("Incoherent symbol type, should be class")

                }

            case _ =>
        }

        true
    }
}
