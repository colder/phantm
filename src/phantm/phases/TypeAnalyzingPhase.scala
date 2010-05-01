package phantm.phases;

import phantm.Settings
import phantm.cfg.{ASTToCFG}
import phantm.ast.Trees._
import phantm.ast.ASTSimpleTraversal
import phantm.symbols._
import phantm.types.TypeFlowAnalyzer

object TypeAnalyzingPhase extends Phase(Some(APIExportingPhase)) {

    def name = "Typeflow analysis"
    def description = "Analyzing types"

    def run(ctx: PhasesContext): PhasesContext = {
        TypeFlowAnalysis(ctx, ctx.oast.get) execute;
        ctx
    }

}


case class TypeFlowAnalysis(ctx: PhasesContext, node: Tree) extends ASTSimpleTraversal(node) {

    def display(content: String) = {
        if (Settings.get.displayProgress) {
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
            case Program(stmts) if filter("main") =>
                display("Converting main scope...")
                val cfg = ASTToCFG.convertAST(stmts, GlobalSymbols)
                display("Analyzing main...")
                val tfa = new TypeFlowAnalyzer(cfg, GlobalSymbols, ctx)
                tfa.analyze


            case FunctionDecl(name, args, retref, body) if filter(name.value) =>
                name.getSymbol match {
                    case fs: FunctionSymbol =>
                        display("Converting function "+name.value+"...")
                        val cfg = ASTToCFG.convertAST(List(body), fs)
                        display("Analyzing function "+name.value+"...")
                        val tfa = new TypeFlowAnalyzer(cfg, fs, ctx)
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
                                    if (filter(cl.name+"::"+m.name.value)) {
                                        display("Converting method "+cl.name+"::"+m.name.value+"...")
                                        val cfg = ASTToCFG.convertAST(List(m.body.get), ms)
                                        display("Analyzing method "+cl.name+"::"+m.name.value+"...")
                                        val tfa = new TypeFlowAnalyzer(cfg, ms, ctx)
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
