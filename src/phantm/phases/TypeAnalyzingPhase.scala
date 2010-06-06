package phantm.phases;

import phantm.Settings
import phantm.cfg.{ASTToCFG}
import phantm.ast.Trees._
import phantm.cfg.{Trees => CFG}
import phantm.ast.ASTSimpleTraversal
import phantm.util.Reporter
import phantm.symbols._
import phantm.types.{Type,TypeFlowAnalyzer}
import phantm.cfg.ControlFlowGraph

object TypeAnalyzingPhase extends Phase {

    def name = "Typeflow analysis"
    def description = "Analyzing types"

    def run(ctx: PhasesContext): PhasesContext = {
        // Print summary header, if requested
        if (Settings.get.summaryOnly) {
            printf(" %3s | %3s | %4s | %3s | %-50s | %s \n", "#N", "#L", "R", "An?", "Symbol:", "File:");
        }
        // If the --only option is used, we're not interrested in the previous errors
        if (Settings.get.typeFlowFilter != Nil) {
            Reporter.get.clear
        }
        val tfa = TypeFlowAnalysis(ctx, ctx.oast.get)
        tfa.execute
        tfa.ctx
    }

}


case class TypeFlowAnalysis(initCtx: PhasesContext, node: Tree) extends ASTSimpleTraversal(node) {

    var ctx = initCtx

    def getCFG(sym: Option[FunctionSymbol]): ControlFlowGraph = {
        ctx.cfgs.get(sym).getOrElse(error("Unknown CFG: "+sym))
    }

    def display(content: String) = {
        if (Settings.get.displayProgress && Settings.get.verbosity > 2) {
            println("     - "+content)
        }
    }

    def filter(name: String): Boolean = {
        ((Settings.get.typeFlowFilter == Nil) || (Settings.get.typeFlowFilter.contains(name))) &&
        (name != "phantm_dumpanddie") &&
        (name != "phantm_incl")
    }

    def visit(node: Tree): Boolean = {
        node match {
            case Program(stmts) =>
                display("Analyzing main...")
                val cfg = getCFG(None)
                val tfa = new TypeFlowAnalyzer(cfg, GlobalSymbols, ctx)
                val results = tfa.analyze

                ctx = ctx.copy(globals = Some(results(cfg.exit).getGlobalsType))

                if (!filter("main")) {
                    Reporter.get.clear
                }

            case FunctionDecl(name, args, retref, body) if filter(name.value) =>
                name.getSymbol match {
                    case fs: FunctionSymbol =>
                        display("Analyzing function "+name.value+"...")
                        val tfa = new TypeFlowAnalyzer(getCFG(Some(fs)), fs, ctx)
                        tfa.analyze
                    case _ =>
                        error("Incoherent symbol type, should be function")
                }


            case ClassDecl(name, flags, parent, interfaces, methods, static_props, props, consts) =>
                name.getSymbol match {
                    case cl: ClassSymbol =>
                        for (m <- methods) if (m.body != None) {
                            m.name.getSymbol match {
                                case ms: MethodSymbol =>
                                    if (filter(cl.name+"::"+m.name.value) || filter(cl.name+"::_")) {
                                        display("Analyzing method "+cl.name+"::"+m.name.value+"...")
                                        val tfa = new TypeFlowAnalyzer(getCFG(Some(ms)), ms, ctx)
                                        tfa.analyze
                                    }
                                case _ =>
                                    error("Incoherent symbol type, should be method")
                            }
                        }
                    case _ =>
                        error("Incoherent symbol type, should be class")

                }

            case _ =>
        }

        true
    }
}
