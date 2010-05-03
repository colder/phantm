package phantm.cfg
import phantm.symbols._
import phantm.ast.{Trees => AST}
import phantm.util.Positional

object Trees {

  sealed abstract class Tree extends Positional {
    override def toString = stringRepr(this)
  }

  var nextID = 0;

  def nextStatementID = {
    nextID += 1
    nextID
  }

  sealed abstract class Statement extends Tree {
    self =>
    var uniqueID: Int = nextStatementID
  }

  case class Unset(variable: Variable) extends Statement
  case class Assign(variable: Variable, value: SimpleValue) extends Statement

  case class AssignUnary(variable: Variable,
                            unOp: UnaryOperator,
                            expr: SimpleValue) extends Statement

  case class AssignBinary(variable: Variable,
                             lhs: SimpleValue,
                             binOp: BinaryOperator,
                             rhs: SimpleValue) extends Statement


  case class Error() extends Statement {
    override def toString = stringRepr(this);
  }

  case class Assume(lhs: SimpleValue, relOp: RelationalOperator, rhs: SimpleValue) extends Statement
  case class Print(rhs: SimpleValue) extends Statement
  case class Return(rhs: SimpleValue) extends Statement

  case object Skip extends Statement

  sealed abstract class Expression extends Statement
  sealed abstract class SimpleValue extends Expression
  sealed abstract class Variable extends SimpleValue
  sealed abstract class SimpleVariable extends Variable

  /** Used to represent the identifiers from the original program. */
  case class Identifier(symbol: VariableSymbol) extends SimpleVariable with Symbolic {
    override def getSymbol = symbol
    override def setSymbol(s: Symbol) = this
    override def toString = stringRepr(this)
  }

  /** Used to represent intermediate values (fresh identifiers). */
  case class TempID(value: java.lang.String) extends SimpleVariable
  case class ClassProperty(symbol: PropertySymbol) extends SimpleVariable

  case class VariableVar(v: SimpleValue) extends Variable
  case class ArrayEntry(arr: SimpleValue, index: SimpleValue) extends Variable
  case class NextArrayEntry(arr: SimpleValue) extends Variable
  case class ObjectProperty(obj: SimpleValue, index: SimpleValue) extends Variable
  case class VariableClassProperty(cl: AST.ClassRef, index: SimpleValue) extends Variable
  case class NoVar() extends Variable

  case class PHPLong(value: Long) extends SimpleValue
  case class PHPFloat(value: Float) extends SimpleValue
  case class PHPString(value: java.lang.String) extends SimpleValue
  case class PHPTrue() extends SimpleValue
  case class PHPAny() extends SimpleValue
  case class PHPFalse() extends SimpleValue
  case class PHPNull() extends SimpleValue
  case class PHPThis() extends SimpleValue
  case class PHPEmptyArray() extends SimpleValue

  case class Instanceof(lhs: SimpleValue, cl: AST.ClassRef) extends SimpleValue
  case class Cast(to: AST.CastType, e: SimpleValue) extends SimpleValue
  case class ArrayNext(ar: SimpleValue) extends SimpleValue
  case class ArrayCurElement(ar: SimpleValue) extends SimpleValue
  case class ArrayCurKey(ar: SimpleValue) extends SimpleValue
  case class ArrayCurIsValid(ar: SimpleValue) extends SimpleValue

  case class Constant(cs: ConstantSymbol) extends SimpleValue
  case class ClassConstant(cs: ClassConstantSymbol) extends SimpleValue
  case class VariableClassConstant(cl: AST.ClassRef, name: AST.Identifier) extends SimpleValue

  case class Ternary(cond: SimpleValue,
                         then: SimpleValue,
                         elze: SimpleValue) extends SimpleValue

  case class FunctionCall(id: AST.Identifier,
                             params: List[SimpleValue]) extends SimpleValue

  case class StaticMethodCall(cl: AST.ClassRef,
                                 id: AST.Identifier,
                                 params: List[SimpleValue]) extends SimpleValue

  case class MethodCall(receiver: SimpleValue,
                                 id: AST.Identifier,
                                 params: List[SimpleValue]) extends SimpleValue

  case class New(cl: AST.ClassRef, params: List[SimpleValue]) extends SimpleValue
  case class Clone(obj: SimpleValue) extends SimpleValue

  sealed abstract class BinaryOperator
  sealed trait RelationalOperator

  case object PLUS extends BinaryOperator { override def toString = "+" }
  case object MINUS extends BinaryOperator { override def toString = "-" }
  case object MULT extends BinaryOperator { override def toString = "*" }
  case object DIV extends BinaryOperator { override def toString = "/" }
  case object CONCAT extends BinaryOperator { override def toString = "." }
  case object MOD extends BinaryOperator { override def toString = "%" }
  case object INSTANCEOF extends BinaryOperator { override def toString = "instanceof" }

  case object BOOLEANAND extends BinaryOperator { override def toString = "&&" }
  case object BOOLEANOR extends BinaryOperator { override def toString = "||" }
  case object BOOLEANXOR extends BinaryOperator { override def toString = "xor" }

  case object BITWISEAND extends BinaryOperator { override def toString = "&" }
  case object BITWISEOR extends BinaryOperator { override def toString = "|" }
  case object BITWISEXOR extends BinaryOperator { override def toString = "^" }

  case object SHIFTLEFT extends BinaryOperator { override def toString = "<<" }
  case object SHIFTRIGHT extends BinaryOperator { override def toString = ">>" }

  case object LT extends BinaryOperator with RelationalOperator { override def toString = "<" }
  case object LEQ extends BinaryOperator with RelationalOperator { override def toString = "<=" }
  case object GEQ extends BinaryOperator with RelationalOperator { override def toString = ">=" }
  case object GT extends BinaryOperator with RelationalOperator { override def toString = ">" }

  case object EQUALS extends BinaryOperator with RelationalOperator { override def toString = "==" }
  case object IDENTICAL extends BinaryOperator with RelationalOperator { override def toString = "===" }
  case object NOTEQUALS extends BinaryOperator with RelationalOperator { override def toString = "!=" }
  case object NOTIDENTICAL extends BinaryOperator with RelationalOperator { override def toString = "!==" }

  sealed abstract class UnaryOperator
  case object BOOLEANNOT extends UnaryOperator { override def toString = "!" }
  case object BITSIWENOT extends UnaryOperator { override def toString = "~" }
  case object PREINC extends UnaryOperator { override def toString = "++ (pre)" }
  case object POSTINC extends UnaryOperator { override def toString = "++ (post)" }
  case object PREDEC extends UnaryOperator { override def toString = "-- (pre)" }
  case object POSTDEC extends UnaryOperator { override def toString = "-- (post)" }
  case object SILENCE extends UnaryOperator { override def toString = "@" }

  def stringRepr(tree: Tree): String = {
    val assOp = " := "

    tree match {
      case AssignUnary(v, u, e) => v + assOp + u + e
      case AssignBinary(v, l, b, r) => v + assOp + l + " " + b + " " + r
      case StaticMethodCall(r, mid, p) => r + "::" + mid.value + p.mkString("(", ", ", ")")
      case MethodCall(r, mid, p) => r + "->" + mid.value + p.mkString("(", ", ", ")")
      case FunctionCall(fid, p) => fid.value + p.mkString("(", ", ", ")")
      case Constant(cs) => cs.name
      case ClassConstant(cs) => cs.cs.name + "::" + cs.name
      case VariableClassConstant(cl, cid) => cl + "::" + cid.value
      case Ternary(i, then, elze) => i + " ? " + then + " : " + elze
      case Assign(v, e) => v + assOp + e
      case Cast(to, e) => "("+to+")" + e
      case Skip => "..."
      case Assume(l, o, r) => "[" + l + o + r + "]"
      case Print(v) => "print("+v+")"
      case Return(v) => "return("+v+")"
      case Unset(v) => "unset("+v+")"
      case PHPString(value) => "\"" + value + "\""
      case PHPLong(value) => value.toString
      case PHPFloat(value) => value.toString
      case New(tpe, params) => "new " + tpe + params.mkString("(", ", ", ")")
      case Clone(obj) => "clone " + obj
      case PHPTrue() => "true"
      case PHPNull() => "null"
      case PHPEmptyArray() => "array()"
      case PHPFalse() => "false"
      case PHPAny() => "any"
      case NoVar() => "none"
      case Error() => "error"
      case PHPThis() => "this"
      case ArrayNext(a) => a + ".next"
      case ArrayCurKey(a) => a + ".key"
      case ArrayCurElement(a) => a + ".current"
      case ArrayCurIsValid(a) => a + ".valid"
      case Instanceof(obj, AST.StaticClassRef(_, _, id)) => obj + " instanceof "+id.value
      case Instanceof(obj, _) => obj + " instanceof ?"
      case Identifier(sym) => sym.name
      case TempID(value) => value
      case VariableVar(v) => "*("+v+")"
      case ArrayEntry(arr, index) => arr+"["+index+"]"
      case NextArrayEntry(arr) => arr+"[]"
      case ObjectProperty(obj, prop) => obj+"->"+prop;
      case ClassProperty(sym) => sym.cs.name+"::$"+sym.name;
      case VariableClassProperty(cl, prop) => cl+"::$"+prop;
    }
  }
}
