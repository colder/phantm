package phantm.AST

import phantm.Positional
import phantm.CommentAnnotation
import phantm.analyzer.Symbols.Symbolic

// todo, namespaces
object Trees {
    abstract class Tree extends Positional with CommentAnnotation;

    case class Program(stmts: List[Statement]) extends Tree {
        def combine(p2: Program): Program = Program(stmts ::: p2.stmts)
    }
    case class ArgumentDecl(v: SimpleVariable, hint: Option[TypeHint], default: Option[Expression], byref: Boolean) extends Tree;
    case class MethodDecl(name: Identifier, flags: List[MemberFlag], args: List[ArgumentDecl], retref: Boolean, body: Option[Statement]) extends Tree
    case class PropertyDecl(v: Identifier, flags: List[MemberFlag], default: Option[Expression]) extends Tree;
    case class ConstantDecl(v: Identifier, value: Expression) extends Tree;

    abstract class ClassFlag extends Tree
    case object CFNormal extends ClassFlag
    case object CFAbstract extends ClassFlag
    case object CFFinal extends ClassFlag

    abstract class TypeHint extends Tree
    case object THArray     extends TypeHint
    case class THObject(cl: ClassRef) extends TypeHint

    abstract class MemberFlag extends Tree
    case object MFAbstract extends MemberFlag
    case object MFPublic extends MemberFlag
    case object MFProtected extends MemberFlag
    case object MFPrivate extends MemberFlag
    case object MFFinal extends MemberFlag
    case object MFStatic extends MemberFlag

    abstract class NSRoot extends Tree
    case object NSNone extends NSRoot /* foo\Bar */
    case object NSGlobal extends NSRoot /* \foo\Bar */
    case object NSCurrent extends NSRoot /* namespace\foo\Bar */

    abstract class ClassRef extends Tree
    case class VarClassRef(v: Variable) extends ClassRef
    case class DynamicClassRef(ex: Expression) extends ClassRef
    case class StaticClassRef(nsroot: NSRoot, nss: List[Identifier], name: Identifier) extends ClassRef with Symbolic
    case class CalledClass() extends ClassRef

    abstract class FunctionRef extends Tree
    case class VarFunctionRef(v: Variable) extends FunctionRef
    case class DynamicFunctionRef(ex: Expression) extends FunctionRef
    case class StaticFunctionRef(nsroot: NSRoot, nss: List[Identifier], name: Identifier) extends FunctionRef

    abstract class MethodRef extends Tree 
    case class DynamicMethodRef(ex: Expression) extends MethodRef
    case class StaticMethodRef(id: Identifier) extends MethodRef

    abstract class CastType extends Tree
    case object CastInt extends CastType
    case object CastString extends CastType
    case object CastDouble extends CastType
    case object CastArray extends CastType
    case object CastBool extends CastType
    case object CastObject extends CastType
    case object CastUnset extends CastType

    case class InitVariable(v: Variable, init: Option[Expression]) extends Tree

    case class Label(name: Identifier) extends Tree
    case class Identifier(value: String) extends Tree with Symbolic

    case class CallArg(value: Expression, forceref: Boolean) extends Tree

    abstract class ObjectAccess extends Tree
    abstract class OAScalar extends ObjectAccess
    case class OAIdentifier(id: Identifier) extends OAScalar
    case class OAExpression(exp: Expression) extends OAScalar
    case class OAArray(array: OAScalar, indexes: List[Option[Expression]]) extends ObjectAccess
    case class OAMethod(name: ObjectAccess, args: List[CallArg]) extends ObjectAccess

    abstract class Statement extends Tree;

    case class FunctionDecl(name: Identifier, args: List[ArgumentDecl], retref: Boolean, body: Statement) extends Statement

    case class ClassDecl(name: Identifier,
                         flags: ClassFlag,
                         parent: Option[StaticClassRef],
                         interfaces: List[StaticClassRef],
                         methods: List[MethodDecl],
                         static_props: List[PropertyDecl],
                         props: List[PropertyDecl],
                         consts: List[ConstantDecl]) extends Statement

    case class InterfaceDecl(name: Identifier,
                         interfaces: List[ClassRef],
                         methods: List[MethodDecl],
                         consts: List[ConstantDecl]) extends Statement

    case class Try(body: Statement, catches: List[Catch]) extends Statement
    case class Catch(cl: ClassRef, v: SimpleVariable, body: Statement) extends Tree
    case class Throw(ex: Expression) extends Statement
    case class Goto(to: Label) extends Statement

    case class LabelDecl(name: Identifier) extends Statement

    case class Block(stmts: List[Statement]) extends Expression // Hack to allow include statments to be resolved
    case class If(cond: Expression, then: Statement, elze: Option[Statement]) extends Statement
    case class While(cond: Expression, then: Statement) extends Statement
    case class DoWhile(body: Statement, cond: Expression) extends Statement
    case class For(init: Statement, cond: Expression, step: Statement, then: Statement) extends Statement
    case class Switch(expr: Expression, cases: List[(Option[Expression], Statement)]) extends Statement
    case class Break(level: Expression) extends Statement
    case class Continue(level: Expression) extends Statement
    case class Return(expr: Expression) extends Statement
    case class Global(vars: List[Variable]) extends Statement
    case class Static(vars: List[InitVariable]) extends Statement
    case class Echo(exprs: List[Expression]) extends Statement
    case class Html(content: String) extends Statement
    case class Unset(vars: List[Variable]) extends Statement
    case class Foreach(what: Expression, as: Variable, asbyref: Boolean, key: Option[Variable], keybyref: Boolean, body: Statement) extends Statement
    case class Void() extends Statement;

    abstract class Expression extends Statement;
    abstract class Variable extends Expression;
    case class SimpleVariable(name: Identifier) extends Variable
    case class VariableVariable(name: Expression) extends Variable
    case class ArrayEntry(array: Expression, index: Expression) extends Variable
    case class NextArrayEntry(array: Expression) extends Variable
    case class ObjectProperty(obj: Expression, property: Identifier) extends Variable
    case class DynamicObjectProperty(obj: Expression, property: Expression) extends Variable
    case class ClassProperty(cl: ClassRef, property: Variable) extends Variable

    // Special for list(list(..)..)
    case class ListVar(vars: List[Option[Variable]]) extends Variable

    case class ExpandArray(vars: List[Option[Variable]], expr: Expression) extends Expression
    case class Assign(vari: Variable, value: Expression, byref: Boolean) extends Expression
    case class Clone(obj: Expression) extends Expression
    case class Plus(lhs: Expression, rhs: Expression) extends Expression
    case class Minus(lhs: Expression, rhs: Expression) extends Expression
    case class Div(lhs: Expression, rhs: Expression) extends Expression
    case class Mult(lhs: Expression, rhs: Expression) extends Expression
    case class Concat(lhs: Expression, rhs: Expression) extends Expression
    case class Mod(lhs: Expression, rhs: Expression) extends Expression
    case class PreInc(rhs: Variable) extends Expression
    case class PostInc(rhs: Variable) extends Expression
    case class PreDec(rhs: Variable) extends Expression
    case class PostDec(rhs: Variable) extends Expression
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
    case class Ternary(cond: Expression, then: Option[Expression], elze: Expression) extends Expression
    case class Cast(typ: CastType, value: Expression) extends Expression
    case class Silence(value: Expression) extends Expression
    case class Exit(value: Option[Expression]) extends Expression
    case class Array(values: List[(Option[Expression],Expression,Boolean)]) extends Expression
    case class Execute(value: String) extends Expression
    case class Print(value: Expression) extends Expression
    case class Eval(value: Expression) extends Expression
    case class Closure(args: List[ArgumentDecl], imports: List[ArgumentDecl], retref: Boolean, body: Statement) extends Expression
    case class Isset(vs: List[Variable]) extends Expression
    case class Empty(v: Variable) extends Expression
    case class Include(path: Expression, once: Boolean) extends Expression
    case class Require(path: Expression, once: Boolean) extends Expression
    case class Constant(name: Identifier) extends Expression
    case class ClassConstant(cl: ClassRef, const: Identifier) extends Expression
    case class New(cl: ClassRef, args: List[CallArg]) extends Expression
    case class FunctionCall(name: FunctionRef, args: List[CallArg]) extends Expression
    case class MethodCall(obj: Expression, name: MethodRef, args: List[CallArg]) extends Expression
    case class StaticMethodCall(cl: ClassRef, name: MethodRef, args: List[CallArg]) extends Expression


    abstract class Scalar extends Expression
    case class PHPTrue() extends Scalar
    case class PHPFalse() extends Scalar
    case class PHPInteger(value: Long) extends Scalar
    case class PHPFloat(value: Float) extends Scalar
    case class PHPString(value: String) extends Scalar
    case class PHPNull() extends Scalar

    // Magic constants
    case class MCFile() extends Scalar
    case class MCLine() extends Scalar
    case class MCDir() extends Scalar
    case class MCClass() extends Scalar
    case class MCFunction() extends Scalar
    case class MCMethod() extends Scalar
    case class MCNamespace() extends Scalar

}
