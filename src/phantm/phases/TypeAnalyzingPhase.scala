package phantm.phases;

import phantm.Settings
import phantm.cfg.{ASTToCFG}
import phantm.ast.Trees._
import phantm.annotations.SourceAnnotations
import phantm.cfg.{Trees => CFG}
import phantm.ast.ASTSimpleTraversal
import phantm.util._
import phantm.symbols._
import phantm.types.{Type,TypeFlowAnalyzer}
import phantm.cfg.ControlFlowGraph

object TypeAnalyzingPhase extends Phase {

    def name = "Typeflow analysis"
    def description = "Analyzing types"

    def run(ctx: PhasesContext): PhasesContext = {
        // If the --only option is used, we're not interrested in the previous errors
        if (Settings.get.typeFlowFilter != Nil) {
            Reporter.get.clear
        }

        if (Settings.get.displayProgress && Settings.get.verbosity <= 2) {
            Reporter.get.beginTicks
        }

        val tfa = TypeFlowAnalysis(ctx, ctx.oast.get)
        tfa.execute

        if (Settings.get.displayProgress && Settings.get.verbosity <= 2) {
            Reporter.get.endTicks
        }

        if (Settings.get.summaryOnly) {
            printf(" %3s | %3s | %4s | %3s | %3s | %-50s | %s \n", "#N", "#L", "R", "An?", "In?", "Symbol:", "File:");

            def limitFileName(s: String) = {
                if (s.length > 30) {
                    "..."+s.substring(s.length-30, s.length-1)
                } else {
                    s
                }
            }

            def displaySummary(fs: FunctionSymbol, noticesCount: Int, name: String) = {
                val lineCount = fs.line_end-fs.line+1;
                val isAnnotated = SourceAnnotations.Parser.isAnnotated(fs.comment.getOrElse(""))
                printf(" %3d | %3d | %.2f | %3s | %3s | %-50s | %s \n", noticesCount,
                                                                  lineCount,
                                                                  noticesCount*1.0/lineCount,
                                                                  if (isAnnotated) "yes" else "no",
                                                                  if (fs.shouldInline) "yes" else "no",
                                                                  name,
                                                                  limitFileName(fs.file.map(f => f+":"+fs.line).getOrElse("-- no file --")));
            }

            for ((fs, nc) <- ctx.results.summary) {
                val name = fs match {
                    case ms: MethodSymbol =>
                        ms.cs.name+"::"+ms.name
                    case fs: FunctionSymbol =>
                        fs.name
                }

                displaySummary(fs, nc, name)
            }
        }


        tfa.ctx
    }

}


case class TypeFlowAnalysis(initCtx: PhasesContext, node: Tree) extends ASTSimpleTraversal(node) {

    var ctx = initCtx

    def getCFG(sym: Option[FunctionSymbol]): ControlFlowGraph = {
        ctx.cfgs.get(sym).getOrElse(error("Unknown CFG: "+sym))
    }

    def display(content: String) = {
        if (Settings.get.displayProgress) {
            if (Settings.get.verbosity > 2) {
                println("     - "+content)
            } else {
                Reporter.get.tick
            }
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
                val tfa = new TypeFlowAnalyzer(cfg, GlobalSymbols, ctx, collectGlobals = true)
                val results = tfa.analyze

                // get exit TypeEnvironment
                ctx.results.endGlobals = Some(results(cfg.exit))

                if (!filter("main")) {
                    Reporter.get.clear
                }

            case FunctionDecl(name, args, retref, body) if filter(name.value) =>
                name.getSymbol match {
                    case fs: FunctionSymbol =>
                        if (!fs.shouldInline) {
                            display("Analyzing function "+name.value+"...")
                            val tfa = new TypeFlowAnalyzer(getCFG(Some(fs)), fs, ctx)
                            tfa.analyze
                        }
                    case _ =>
                        error("Incoherent symbol type, should be function")
                }


            case ClassDecl(name, flags, parent, interfaces, methods, static_props, props, consts) =>
                name.getSymbol match {
                    case cl: ClassSymbol =>
                        for (m <- methods) if (m.body != None) {
                            m.name.getSymbol match {
                                case ms: MethodSymbol =>
                                    if (!ms.shouldInline && (filter(cl.name+"::"+m.name.value) || filter(cl.name+"::_"))) {
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
