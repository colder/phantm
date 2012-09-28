package phantm.phases

import phantm.Settings
import phantm.symbols._
import phantm.ast.Trees._
import phantm.ast.ASTSimpleTraversal
import phantm.util.Reporter

object PureStatementsPhase  extends Phase {

    def name = "Pure statements checks"
    def description = "Checking for pure statements"

    def run(ctx: PhasesContext): PhasesContext = {
        new PureStatementsChecks(ctx.oast.get, ctx) execute;
        ctx
    }

}

case class PureStatementsChecks(node: Tree, ctx: PhasesContext) extends ASTSimpleTraversal(node) {

    def checkPures(stmts: List[Statement]) = {
        stmts foreach (checkPure _)
    }

    def isPure(stmt: Statement): Boolean = stmt match {
        case ex: Expression => ex match {
            case Block(_) =>
                false
            case DynamicObjectProperty(obj: Expression, property: Expression) =>
                isPure(obj) && isPure(property)
            case ObjectProperty(obj: Expression, property: Identifier) =>
                isPure(obj)
            case NextArrayEntry(array: Expression) =>
                isPure(array)
            case SimpleVariable(_) =>
                true
            case ArrayEntry(array: Expression, index: Expression) =>
                isPure(array) && isPure(index)
            case VariableVariable(name: Expression) =>
                isPure(name)
            case ClassProperty(cl: ClassRef, property: Variable) =>
                isPure(property)
            case ExpandArray(vars, expr) =>
                false
            case Assign(vari: Variable, value: Expression, byref: Boolean) =>
                false
            case Clone(obj: Expression) =>
                isPure(obj)
            case Plus(lhs: Expression, rhs: Expression) =>
                isPure(lhs) && isPure(rhs)
            case Minus(lhs: Expression, rhs: Expression) =>
                isPure(lhs) && isPure(rhs)
            case Div(lhs: Expression, rhs: Expression) =>
                isPure(lhs) && isPure(rhs)
            case Mult(lhs: Expression, rhs: Expression) =>
                isPure(lhs) && isPure(rhs)
            case Concat(lhs: Expression, rhs: Expression) =>
                isPure(lhs) && isPure(rhs)
            case Mod(lhs: Expression, rhs: Expression) =>
                isPure(lhs) && isPure(rhs)
            case PreInc(rhs: Variable) =>
                false
            case PostInc(rhs: Variable) =>
                false
            case PreDec(rhs: Variable) =>
                false
            case PostDec(rhs: Variable) =>
                false
            case BooleanAnd(lhs: Expression, rhs: Expression) =>
                isPure(lhs) && isPure(rhs)
            case BooleanOr(lhs: Expression, rhs: Expression)  =>
                isPure(lhs) && isPure(rhs)
            case BooleanXor(lhs: Expression, rhs: Expression) =>
                isPure(lhs) && isPure(rhs)
            case BitwiseAnd(lhs: Expression, rhs: Expression) =>
                isPure(lhs) && isPure(rhs)
            case BitwiseOr(lhs: Expression, rhs: Expression)  =>
                isPure(lhs) && isPure(rhs)
            case BitwiseXor(lhs: Expression, rhs: Expression) =>
                isPure(lhs) && isPure(rhs)
            case ShiftLeft(lhs: Expression, rhs: Expression)  =>
                isPure(lhs) && isPure(rhs)
            case ShiftRight(lhs: Expression, rhs: Expression) =>
                isPure(lhs) && isPure(rhs)
            case BooleanNot(rhs: Expression) =>
                isPure(rhs)
            case BitwiseNot(rhs: Expression) =>
                isPure(rhs)
            case Equal(lhs: Expression, rhs: Expression) =>
                isPure(lhs) && isPure(rhs)
            case Identical(lhs: Expression, rhs: Expression) =>
                isPure(lhs) && isPure(rhs)
            case Smaller(lhs: Expression, rhs: Expression) =>
                isPure(lhs) && isPure(rhs)
            case SmallerEqual(lhs: Expression, rhs: Expression) =>
                isPure(lhs) && isPure(rhs)
            case InstanceOf(lhs: Expression, rhs: ClassRef) =>
                isPure(lhs)
            case Ternary(cond, then, elze) =>
                isPure(cond) && isPure(elze) && (then == None || isPure(then.get))
            case Cast(typ: CastType, value: Expression) =>
                isPure(value)
            case Silence(value) =>
                isPure(value)
            case Exit(_) =>
                false
            case Array(values) =>
                values.forall(v => isPure(v._2) && (v._1 == None || isPure(v._1.get)))
            case Execute(value: String) =>
                false
            case Print(value: Expression) =>
                false
            case Eval(value: Expression) =>
                false
            case Closure(args, imports, retref, body) =>
                true
            case Isset(vs) =>
                vs.forall(isPure _)
            case Empty(v: Variable) =>
                isPure(v)
            case Include(path: Expression, once: Boolean) =>
                false
            case Require(path: Expression, once: Boolean) =>
                false
            case Constant(name: Identifier) =>
                true
            case ClassConstant(cl: ClassRef, const: Identifier) =>
                true
            case New(_, _) =>
                false
            case FunctionCall(StaticFunctionRef(_, _, id), args) =>
                ctx.globalSymbols.lookupFunction(id.value) match {
                  case Some(fs) =>
                    if (fs.isPure) {
                        args.forall(a => isPure(a.value))
                    } else {
                        false
                    }
                  case None =>
                    false
                }
            case FunctionCall(_, _) =>
                false
            case MethodCall(_, _, _) =>
                false
            case StaticMethodCall(_, _, _) =>
                false
            case VoidExpr() =>
                false
            case _: Scalar =>
                true

        }
        case _ => false
    }

    def checkPure(stmt: Statement) = {
        if (isPure(stmt)) {
            Reporter.notice("Statement with no side-effect", stmt)
        }
    }

    def visit(tr: Tree): Boolean = {
        tr match {
            case Block(stmts) =>
                checkPures(stmts)
            case FunctionDecl(_, _, _, stmt) =>
                checkPure(stmt)
            case Try(stmt, _) =>
                checkPure(stmt)
            case If(_, then, elze) =>
                checkPure(then)
                if (elze != None) {
                    checkPure(elze.get)
                }
            case While(_, then: Statement) =>
                checkPure(then)
            case DoWhile(stmt, _) =>
                checkPure(stmt)
            case For(stmt1: Statement, _, stmt2: Statement, stmt3: Statement) =>
                checkPure(stmt1)
                checkPure(stmt2)
                checkPure(stmt3)
            case Foreach(_, _, _, _, _, stmt) =>
                checkPure(stmt)
            case Program(stmts) =>
                checkPures(stmts)
            case _ =>
        }
        true
    }

}

