package phantm.phases

import phantm.Settings
import phantm.ast.Trees._
import phantm.ast.{ASTTransform, ASTSimpleTraversal}
import phantm.util.Reporter

object ASTPruningPhase extends Phase(Some(ASTChecksPhase)) {

    def name = "AST pruning"
    def description = "Prunes AST before dump and die"

    def run(ctx: PhasesContext): PhasesContext = {
        if (ctx.dumpedData != Nil) {

            val functions = ctx.dumpedData.flatMap(dc => dc.functions).toMap
            val classes   = ctx.dumpedData.flatMap(dc => dc.classes).toMap

            // First of all, we collect every class and functions defined in the AST
            val astC = new ASTCollector(functions, classes, ctx.oast.get)
            astC.execute

            // Prune the AST until dump_and_die
            var ast = new ASTPruner(ctx.oast.get).transform

            // Backpatch classes and functions definitions
            ctx.copy(oast = Some(Program(astC.functionsDecls ::: astC.classesDecls ::: ast.stmts)))
        } else {
            ctx
        }
    }

}

class ASTCollector(functs: Map[String, (String, Int)],
                   classes: Map[String, (String, Int)],
                   ast: Program) extends ASTSimpleTraversal(ast) {

    var functionsDecls = List[FunctionDecl]()
    var classesDecls   = List[ClassDecl]()

    def visit(t: Tree): Boolean = {
        t match {
            case fd @ FunctionDecl(Identifier(name), _, _, _) =>
                if (functs contains name) {
                    val (file, line) = functs(name)
                    if (file == fd.file.get && line == fd.line) {
                        functionsDecls = fd :: functionsDecls
                    }
                }
            case cd @ ClassDecl(Identifier(name), _, _, _, _, _, _, _) =>
                if (classes contains name) {
                    val (file, line) = classes(name)
                    if (file == cd.file.get && line == cd.line) {
                        classesDecls = cd :: classesDecls
                    }
                }
            case _ =>
        }
        true
    }

}

class ASTPruner(ast: Program) extends ASTTransform(ast) {

    var afterDump = false

    override def trStmts(sts: List[Statement]): List[Statement] = super.trStmts(sts).filter(_ != Void())

    override def trStmt(st: Statement): Statement = st match {
        case FunctionCall(StaticFunctionRef(_, _, Identifier("phantm_dumpanddie")), _) =>
            // found the call
            afterDump = true
            st
        case st if !afterDump => Void()
        case st => super.trStmt(st)

    }
}
