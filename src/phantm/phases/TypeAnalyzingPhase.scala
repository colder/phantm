package phantm.phases;

import phantm.Settings
import phantm.cfg.{ASTToCFG}
import phantm.ast.Trees._
import phantm.cfg.{Trees => CFG}
import phantm.ast.ASTSimpleTraversal
import phantm.util.Reporter
import phantm.symbols._
import phantm.types.{Type,TypeFlowAnalyzer}

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
        TypeFlowAnalysis(ctx, ctx.oast.get) execute;
        ctx
    }

}


case class TypeFlowAnalysis(ctx: PhasesContext, node: Tree) extends ASTSimpleTraversal(node) {

    var mainGlobals: Option[Type] = None

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
                display("Converting main scope...")
                val cfg = ASTToCFG.convertAST(stmts, GlobalSymbols)
                display("Analyzing main...")
                val tfa = new TypeFlowAnalyzer(cfg, GlobalSymbols, ctx, None)
                val results = tfa.analyze

                mainGlobals = Some(results(cfg.exit).getGlobalsType)
                if (!filter("main")) {
                    Reporter.get.clear
                }

            case FunctionDecl(name, args, retref, body) if filter(name.value) =>
                name.getSymbol match {
                    case fs: FunctionSymbol =>
                        display("Converting function "+name.value+"...")
                        val cfg = ASTToCFG.convertAST(List(body), fs)
                        display("Analyzing function "+name.value+"...")
                        val tfa = new TypeFlowAnalyzer(cfg, fs, ctx, mainGlobals)
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
                                        display("Converting method "+cl.name+"::"+m.name.value+"...")
                                        val cfg = ASTToCFG.convertAST(List(m.body.get), ms)
                                        display("Analyzing method "+cl.name+"::"+m.name.value+"...")
                                        val tfa = new TypeFlowAnalyzer(cfg, ms, ctx, mainGlobals)
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
