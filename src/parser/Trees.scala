// todo, halt_compiler, namespaces
object Trees {
    abstract class Tree;

    case class Program(stmts: List[Statement]) extends Tree;
    case class ArgumentDecl(v: Identifier, hint: Option[TypeHint], default: Option[Expression], byref: Boolean) extends Tree;
    case class MethodDecl(name: Identifier, flags: List[MethodFlag], args: List[ArgumentDecl], retref: Boolean, body: Option[Statement]) extends Tree
    case class PropertyDecl(v: Identifier, flags: List[PropertyFlag], default: Option[Expression]) extends Tree;
    case class ConstantDecl(v: Identifier, value: Expression) extends Tree;

    abstract class ClassFlag extends Tree
    object CFAbstract extends ClassFlag
    object CFFinal extends ClassFlag

    abstract class TypeHint extends Tree
    object TString extends TypeHint
    object TInt extends TypeHint
    object TBoolean extends TypeHint
    object TFloat extends TypeHint
    object TArray extends TypeHint

    abstract class MethodFlag extends Tree
    object MFAbstract extends MethodFlag
    object MFPublic extends MethodFlag
    object MFProtected extends MethodFlag
    object MFPrivate extends MethodFlag
    object MFFinal extends MethodFlag
    object MFStatic extends MethodFlag

    abstract class PropertyFlag extends Tree
    object PFPublic extends PropertyFlag
    object PFProtected extends PropertyFlag
    object PFPrivate extends PropertyFlag
    object PFStatic extends PropertyFlag

    abstract class ClassRef extends Tree

    abstract class CastType extends Tree
    object CastInt extends CastType
    object CastString extends CastType
    object CastDouble extends CastType
    object CastArray extends CastType
    object CastBool extends CastType
    object CastUnset extends CastType

    case class Identifier(value: String) extends Tree;

    abstract class Statement extends Tree;

    case class FunctionDecl(name: Identifier, args: List[ArgumentDecl], retref: Boolean, body: Statement) extends Statement

    case class ClassDecl(name: Identifier,
                         flags: List[ClassFlag],
                         parent: Option[Identifier],
                         interfaces: List[Identifier],
                         methods: List[MethodDecl],
                         static_props: List[PropertyDecl],
                         props: List[PropertyDecl],
                         consts: List[ConstantDecl]) extends Statement

    case class Try(body: Statement, catches: List[Catch]) extends Statement
    case class Catch(cl: Identifier, v: Variable, body: Statement) extends Statement
    case class Throw(ex: Expression) extends Statement
    case class Goto(to: Label) extends Statement

    case class Label(name: String) extends Statement

    case class Block(stmts: List[Statement]) extends Statement
    case class If(cond: Expression, then: Statement, elze: Statement) extends Statement
    case class While(cond: Expression, then: Statement) extends Statement
    case class DoWhile(cond: Expression, then: Statement) extends Statement
    case class For(init: List[Expression], cond: List[Expression], step: List[Expression], then: Statement) extends Statement
    case class Switch(expr: Expression, cases: List[(Expression, List[Statement])], default: Option[List[Statement]]) extends Statement
    case class Break(level: Expression) extends Statement
    case class Continue(level: Expression) extends Statement
    case class Return(expr: Expression) extends Statement
    case class Global(vars: List[Variable]) extends Statement
    case class Static(vars: List[Variable]) extends Statement
    case class Echo(exprs: List[Expression]) extends Statement
    case class Html(content: String) extends Statement
    case class Unset(vars: List[Variable]) extends Statement
    case class Foreach(what: Expression, as: Variable, key: Option[Variable], body: Statement) extends Statement

    abstract class Expression;
    case class Variable(name: Identifier) extends Expression
    case class ExpandArray(vars: List[Variable], expr: Expression) extends Expression
    case class Assign(vari: Variable, value: Expression, byref: Boolean) extends Expression
    case class New(cl: ClassRef, args: List[Expression]) extends Expression
    case class Clone(obj: Expression) extends Expression
    case class Plus(lhs: Expression, rhs: Expression) extends Expression
    case class Minus(lhs: Expression, rhs: Expression) extends Expression
    case class Div(lhs: Expression, rhs: Expression) extends Expression
    case class Mult(lhs: Expression, rhs: Expression) extends Expression
    case class Concat(lhs: Expression, rhs: Expression) extends Expression
    case class Mod(lhs: Expression, rhs: Expression) extends Expression
    case class BooleanAnd(lhs: Expression, rhs: Expression) extends Expression
    case class BooleanOr(lhs: Expression, rhs: Expression) extends Expression
    case class BooleanXor(lhs: Expression, rhs: Expression) extends Expression
    case class BitwiseAnd(lhs: Expression, rhs: Expression) extends Expression
    case class BitwiseOr(lhs: Expression, rhs: Expression) extends Expression
    case class BitwiseXor(lhs: Expression, rhs: Expression) extends Expression
    case class ShiftLeft(lhs: Expression, rhs: Expression) extends Expression
    case class ShiftRight(lhs: Expression, rhs: Expression) extends Expression
    case class BooleanNot(rhs: Expression) extends Expression
    case class BitwiseNot(rhs: Expression) extends Expression
    case class Equal(lhs: Expression, rhs: Expression) extends Expression
    case class Identical(lhs: Expression, rhs: Expression) extends Expression
    case class Smaller(lhs: Expression, rhs: Expression) extends Expression
    case class SmallerEqual(lhs: Expression, rhs: Expression) extends Expression
    case class InstanceOf(lhs: Expression, rhs: ClassRef) extends Expression
    case class Ternary(cond: Expression, then: Expression, elze: Expression) extends Expression
    case class Cast(typ: CastType, value: Expression) extends Expression
    case class Silence(value: Expression) extends Expression
    case class Exit(value: Option[Expression]) extends Expression
    case class Array(values: List[(Option[Expression],Expression)]) extends Expression
    case class Execute(value: String) extends Expression
}
