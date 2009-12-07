package phpanalysis.analyzer
import parser.Trees._

abstract class ASTTransform(p: Program) {
    def transform: Program = {
        Program(p.stmts map trStmt)
    }

    def trMethod(md: MethodDecl): MethodDecl = md.body match {
        case Some(b) => 
            MethodDecl(md.name, md.flags, md.args, md.retref, Some(trStmt(b)))
        case None =>
            md
    }

    def trExpr(ex: Expression): Expression = {
        ex
    }

    def trStmt(st: Statement): Statement = st match {
            case FunctionDecl(name, args, retref, body) =>
                FunctionDecl(name, args, retref, trStmt(body))
            case ClassDecl(name, flags, parent, interfaces, methods, static_props, props, consts) =>
                ClassDecl(name, flags, parent, interfaces, methods map trMethod, static_props, props, consts)
            case Try(body, catches) =>
                Try(trStmt(body), catches map { c => Catch(c.cl, c.v, trStmt(c.body) )})
            case Block(stmts) =>
                Block(stmts map trStmt)
            case If(cond, then, elze) =>
                elze match {
                    case Some(e) =>
                        If(trExpr(cond), trStmt(then), Some(trStmt(e)))
                    case None =>
                        If(trExpr(cond), trStmt(then), None)
                }
            case While(cond, then) =>
                While(trExpr(cond), trStmt(then))
            case DoWhile(body, cond) =>
                DoWhile(trStmt(body), trExpr(cond))
            case For(init, cond, step, then) =>
                For(trStmt(init), trExpr(cond), trStmt(step), trStmt(then))
            case Switch(expr, cases) =>
                Switch(trExpr(expr), cases map { c => c match {
                    case (Some(e), st) => (Some(trExpr(e)), trStmt(st))
                    case (None, st)    => (None, trStmt(st))
                }})
            case Break(level) =>
                Break(trExpr(level))
            case Continue(level) =>
                Continue(trExpr(level))
            case Return(expr) =>
                Return(trExpr(expr))
            case Echo(exprs) =>
                Echo(exprs map trExpr)
            case Foreach(what, as, asbyref, key, keybyref, body) =>
                Foreach(trExpr(what), as, asbyref, key, keybyref, trStmt(body))
            case e: Expression =>
                trExpr(e)
    }
}
