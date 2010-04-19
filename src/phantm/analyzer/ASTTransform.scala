package phantm.analyzer
import phantm.parser.Trees._

abstract class ASTTransform(p: Program) {
    def transform: Program = {
        Program(trStmts(p.stmts)).setPos(p)
    }

    def trStmts(stmts: List[Statement]): List[Statement] = stmts match {
        case st :: sts => trStmt(st) :: trStmts(sts)
        case Nil => Nil
    }

    def trMethod(md: MethodDecl): MethodDecl = md.body match {
        case Some(b) => 
            MethodDecl(md.name, md.flags, md.args, md.retref, Some(trStmt(b))).setPos(md).annotateFromC(md)
        case None =>
            md
    }
    def trProperty(pd: PropertyDecl): PropertyDecl = pd match {
        case PropertyDecl(v, flags, Some(default)) =>
            PropertyDecl(v, flags, Some(trExpr(default))).setPos(pd).annotateFromC(pd)
        case PropertyDecl(v, flags, None) =>
            PropertyDecl(v, flags, None).setPos(pd).annotateFromC(pd)
    }

    def trExpr(ex: Expression): Expression = {
        val r = ex match {
            case v: Variable =>
                v
            case s: Scalar =>
                s
            case ExpandArray(vars, expr) =>
                ExpandArray(vars, trExpr(expr))
            case Assign(vari, value, byref) =>
                Assign(vari, trExpr(value), byref)
            case Clone(obj) =>
                Clone(trExpr(obj))
            case Plus(lhs, rhs) =>
                Plus(trExpr(lhs), trExpr(rhs))
            case Minus(lhs, rhs) =>
                Minus(trExpr(lhs), trExpr(rhs))
            case Div(lhs, rhs) =>
                Div(trExpr(lhs), trExpr(rhs))
            case Mult(lhs, rhs) =>
                Mult(trExpr(lhs), trExpr(rhs))
            case Concat(lhs, rhs) =>
                Concat(trExpr(lhs), trExpr(rhs))
            case Mod(lhs, rhs) =>
                Mod(trExpr(lhs), trExpr(rhs))
            case BooleanAnd(lhs, rhs) =>
                BooleanAnd(trExpr(lhs), trExpr(rhs))
            case BooleanOr(lhs, rhs) =>
                BooleanOr(trExpr(lhs), trExpr(rhs))
            case BooleanXor(lhs, rhs) =>
                BooleanXor(trExpr(lhs), trExpr(rhs))
            case BitwiseAnd(lhs, rhs) =>
                BitwiseAnd(trExpr(lhs), trExpr(rhs))
            case BitwiseOr(lhs, rhs) =>
                BitwiseOr(trExpr(lhs), trExpr(rhs))
            case BitwiseXor(lhs, rhs) =>
                BitwiseXor(trExpr(lhs), trExpr(rhs))
            case ShiftLeft(lhs, rhs) =>
                ShiftLeft(trExpr(lhs), trExpr(rhs))
            case ShiftRight(lhs, rhs) =>
                ShiftRight(trExpr(lhs), trExpr(rhs))
            case BooleanNot(rhs) =>
                BooleanNot(trExpr(rhs))
            case BitwiseNot(rhs) =>
                BitwiseNot(trExpr(rhs))
            case Equal(lhs, rhs) =>
                Equal(trExpr(lhs), trExpr(rhs))
            case Identical(lhs, rhs) =>
                Identical(trExpr(lhs), trExpr(rhs))
            case Smaller(lhs, rhs) =>
                Smaller(trExpr(lhs), trExpr(rhs))
            case SmallerEqual(lhs, rhs) =>
                SmallerEqual(trExpr(lhs), trExpr(rhs))
            case InstanceOf(lhs, rhs) =>
                InstanceOf(trExpr(lhs), rhs)
            case Ternary(cond, then, elze) =>
                then match {
                    case Some(th) =>
                        Ternary(trExpr(cond), Some(trExpr(th)), trExpr(elze))
                    case None =>
                        Ternary(trExpr(cond), None, trExpr(elze))
                }
            case Cast(typ: CastType, value) =>
                Cast(typ, trExpr(value))
            case Silence(value) =>
                Silence(trExpr(value))
            case Exit(value) =>
                value match {
                    case Some(v) =>
                        Exit(Some(trExpr(v)))

                    case None =>
                        Exit(None)
                }
            case Print(value) =>
                Print(trExpr(value))
            case Eval(value) =>
                Eval(trExpr(value))
            case ex => ex
        }

        r.setPos(ex).annotateFromC(ex)
    }

    def trStmt(st: Statement): Statement = {
        var r = st match {
            case FunctionDecl(name, args, retref, body) =>
                FunctionDecl(name, args, retref, trStmt(body))
            case ClassDecl(name, flags, parent, interfaces, methods, static_props, props, consts) =>
                ClassDecl(name, flags, parent, interfaces, methods map trMethod, static_props map trProperty, props map trProperty, consts)
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
            case _ =>
                st
        }

        r.setPos(st).annotateFromC(st)
    }
}
