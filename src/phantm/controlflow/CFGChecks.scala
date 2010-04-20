package phantm.controlflow;

import phantm.CFG.{ASTToCFG};
import phantm.AST.Trees._;
import phantm.AST.ASTTraversal;
import phantm.analyzer._;
import phantm.Main;

case class CheckContext();

case class CFGChecks(node: Tree) extends ASTTraversal[CheckContext](node, CheckContext()) {

    def display(content: String) = {
        if (Main.displayProgress) {
            println("     - "+content)
        }
    }

    def filter(name: String): Boolean = {
        ((Main.typeFlowFilter == Nil) || (Main.typeFlowFilter.contains(name))) &&
        (name != "phantm_dumpanddie") &&
        (name != "phantm_incl")
    }

    /**
     * Visit the nodes and aggregate information inside the context to provide
     * hints about obvious errors directly from the AST
     */
    def visit(node: Tree, ctx: CheckContext): (CheckContext, Boolean) = {
        var newCtx = ctx;

        node match {
            case Program(stmts) if filter("main") =>
                display("Converting main scope...")
                val cfg = ASTToCFG.convertAST(stmts, Symbols.GlobalSymbols)
                display("Analyzing main...")
                val tfa = new TypeFlowAnalyzer(cfg, Symbols.GlobalSymbols)
                tfa.analyze


            case FunctionDecl(name, args, retref, body) if filter(name.value) =>
                name.getSymbol match {
                    case fs: Symbols.FunctionSymbol =>
                        display("Converting function "+name.value+"...")
                        val cfg = ASTToCFG.convertAST(List(body), fs)
                        display("Analyzing function "+name.value+"...")
                        val tfa = new TypeFlowAnalyzer(cfg, fs)
                        tfa.analyze
                    case _ =>
                        error("Incoherent symbol type, should be function")
                }


            case ClassDecl(name, flags, parent, interfaces, methods, static_props, props, consts) =>
                name.getSymbol match {
                    case cl: Symbols.ClassSymbol =>
                        for (m <- methods) if (m.body != None) {
                            m.name.getSymbol match {
                                case ms: Symbols.MethodSymbol =>
                                    if (filter(cl.name+"::"+m.name.value)) {
                                        display("Converting method "+cl.name+"::"+m.name.value+"...")
                                        val cfg = ASTToCFG.convertAST(List(m.body.get), ms)
                                        display("Analyzing method "+cl.name+"::"+m.name.value+"...")
                                        val tfa = new TypeFlowAnalyzer(cfg, ms)
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

        (newCtx, true)
    }

    def execute = {
        traverse(visit)
        if (Main.displayProgress) {
            display("All done")
            println
        }
    }
}
