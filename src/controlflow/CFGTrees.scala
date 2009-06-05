package phpanalysis.controlflow

object CFGTrees {
  import analyzer.Symbols._
  import analyzer.Types._

  sealed abstract class CFGTree {
    override def toString = stringRepr(this)
  }

  sealed abstract class CFGStatement extends CFGTree
  case class CFGAssign(variable: CFGVariable, value: CFGSimpleValue) extends CFGStatement

  case class CFGAssignUnary(variable: CFGVariable,
                            unOp: CFGUnaryOperator,
                            expr: CFGSimpleValue) extends CFGStatement

  case class CFGAssignBinary(variable: CFGVariable,
                             lhs: CFGSimpleValue,
                             binOp: CFGBinaryOperator,
                             rhs: CFGSimpleValue) extends CFGStatement

  case class CFGAssignTernary(variable: CFGVariable,
                            test: CFGSimpleValue,
                            then: CFGSimpleValue,
                            elze: CFGSimpleValue) extends CFGStatement

  case class CFGAssignFunctionCall(variable: CFGVariable,
                                 id: parser.Trees.Identifier,
                                 params: List[CFGSimpleValue]) extends CFGStatement

  case class CFGAssignMethodCall(variable: CFGVariable,
                                 receiver: CFGSimpleValue,
                                 id: parser.Trees.Identifier,
                                 params: List[CFGSimpleValue]) extends CFGStatement

  case class CFGArrayAssignNext(arr: CFGVariable,
                            expr: CFGSimpleValue) extends CFGStatement

  case class CFGArrayAssign(arr: CFGVariable,
                            index: CFGSimpleValue,
                            expr: CFGSimpleValue) extends CFGStatement

  case class CFGError() extends CFGStatement with Positional {
    override def toString = stringRepr(this);
  }

  case class CFGAssume(lhs: CFGSimpleValue, relOp: CFGRelationalOperator, rhs: CFGSimpleValue) extends CFGStatement

  case object CFGSkip extends CFGStatement

  sealed abstract class CFGExpression extends CFGTree with Typed
  sealed abstract class CFGSimpleValue extends CFGExpression
  sealed abstract class CFGVariable extends CFGSimpleValue 

  /** Used to represent the identifiers from the original program. */
  case class CFGIdentifier(symbol: VariableSymbol) extends CFGVariable with Symbolic with Positional {
    override def getSymbol = symbol
    override def setSymbol(s: Symbol) = this
    override def getType = symbol.getType
    override def setType(t: Type) = this
    override def toString = stringRepr(this)
  }

  /** Used to represent intermediate values (fresh identifiers). */
  case class CFGTempID(value: String) extends CFGVariable
  case class CFGNumLit(value: Int) extends CFGSimpleValue
  case class CFGStringLit(value: String) extends CFGSimpleValue
  case object CFGTrue extends CFGSimpleValue
  case object CFGFalse extends CFGSimpleValue
  case object CFGNull extends CFGSimpleValue
  case object CFGThis extends CFGSimpleValue
  case object CFGEmptyArray extends CFGSimpleValue
  case class CFGInstanceof(lhs: CFGSimpleValue, cl: parser.Trees.ClassRef) extends CFGSimpleValue
  case class CFGArrayNext(ar: CFGSimpleValue) extends CFGSimpleValue
  case class CFGArrayCurElement(ar: CFGSimpleValue) extends CFGSimpleValue
  case class CFGArrayCurKey(ar: CFGSimpleValue) extends CFGSimpleValue
  case class CFGArrayCurIsValid(ar: CFGSimpleValue) extends CFGSimpleValue

  case class CFGNew(tpe: parser.Trees.Identifier, params: List[CFGSimpleValue]) extends CFGSimpleValue

  sealed abstract class CFGBinaryOperator
  sealed trait CFGRelationalOperator

  case object PLUS extends CFGBinaryOperator { override def toString = "+" }
  case object MINUS extends CFGBinaryOperator { override def toString = "-" }
  case object MULT extends CFGBinaryOperator { override def toString = "*" }
  case object DIV extends CFGBinaryOperator { override def toString = "/" }
  case object CONCAT extends CFGBinaryOperator { override def toString = "." }
  case object MOD extends CFGBinaryOperator { override def toString = "%" }
  case object INSTANCEOF extends CFGBinaryOperator { override def toString = "instanceof" }

  case object BOOLEANAND extends CFGBinaryOperator { override def toString = "&&" }
  case object BOOLEANOR extends CFGBinaryOperator { override def toString = "||" }
  case object BOOLEANXOR extends CFGBinaryOperator { override def toString = "xor" }

  case object BITWISEAND extends CFGBinaryOperator { override def toString = "&" }
  case object BITWISEOR extends CFGBinaryOperator { override def toString = "|" }
  case object BITWISEXOR extends CFGBinaryOperator { override def toString = "^" }

  case object SHIFTLEFT extends CFGBinaryOperator { override def toString = "<<" }
  case object SHIFTRIGHT extends CFGBinaryOperator { override def toString = ">>" }

  case object LT extends CFGBinaryOperator with CFGRelationalOperator { override def toString = "<" }
  case object LEQ extends CFGBinaryOperator with CFGRelationalOperator { override def toString = "<=" }
  case object GEQ extends CFGBinaryOperator with CFGRelationalOperator { override def toString = ">=" }
  case object GT extends CFGBinaryOperator with CFGRelationalOperator { override def toString = ">" }

  case object EQUALS extends CFGBinaryOperator with CFGRelationalOperator { override def toString = "==" }
  case object IDENTICAL extends CFGBinaryOperator with CFGRelationalOperator { override def toString = "===" }
  case object NOTEQUALS extends CFGBinaryOperator with CFGRelationalOperator { override def toString = "!=" }
  case object NOTIDENTICAL extends CFGBinaryOperator with CFGRelationalOperator { override def toString = "!==" }

  case object ARRAYREAD extends CFGBinaryOperator

  sealed abstract class CFGUnaryOperator
  case object BOOLEANNOT extends CFGUnaryOperator { override def toString = "!" }
  case object BITSIWENOT extends CFGUnaryOperator { override def toString = "~" }
  case object CLONE extends CFGUnaryOperator { override def toString = "clone" }
  case object PREINC extends CFGUnaryOperator { override def toString = "++ (pre)" }
  case object POSTINC extends CFGUnaryOperator { override def toString = "++ (post)" }
  case object PREDEC extends CFGUnaryOperator { override def toString = "-- (pre)" }
  case object POSTDEC extends CFGUnaryOperator { override def toString = "-- (post)" }
  case object SILENCE extends CFGUnaryOperator { override def toString = "@" }

  def stringRepr(tree: CFGTree): String = {
    val assOp = " := "

    tree match {
      case CFGAssignMethodCall(v, r, mid, p) => v + assOp + r + "." + mid.value + p.mkString("(", ", ", ")")
      case CFGAssignFunctionCall(v, fid, p) => v + assOp + fid.value + p.mkString("(", ", ", ")")
      case CFGAssignUnary(v, u, e) => v + assOp + u + e
      case CFGAssignBinary(v, l, ARRAYREAD, r) => v + assOp + l + "[" + r + "]"
      case CFGAssignBinary(v, l, b, r) => v + assOp + l + " " + b + " " + r
      case CFGAssignTernary(v, i, then, elze) => v + assOp + i + " ? " + then + " : " + elze
      case CFGAssign(v, e) => v + assOp + e
      case CFGArrayAssign(a, i, e) => a + "[" + i + "]" + assOp + e
      case CFGArrayAssignNext(a, e) => a + "[]" + assOp + e
      case CFGSkip => "..."
      case CFGAssume(l, o, r) => "[" + l + o + r + "]"
      case CFGStringLit(value) => "\\\"" + value + "\\\""
      case CFGNumLit(value) => value.toString
      case CFGNew(tpe, params) => "new " + tpe.value + params.mkString("(", ", ", ")")
      case CFGTrue => "true"
      case CFGNull => "null"
      case CFGEmptyArray => "array()"
      case CFGFalse => "false"
      case CFGError() => "error"
      case CFGThis => "this"
      case CFGArrayNext(a) => a + ".next"
      case CFGArrayCurKey(a) => a + ".key"
      case CFGArrayCurElement(a) => a + ".current"
      case CFGArrayCurIsValid(a) => a + ".valid"
      case CFGInstanceof(obj, parser.Trees.StaticClassRef(_, _, id)) => obj + " instanceof "+id.value
      case CFGInstanceof(obj, _) => obj + " instanceof ?"
      case CFGIdentifier(sym) => sym.name
      case CFGTempID(value) => value
    }
  }
}
