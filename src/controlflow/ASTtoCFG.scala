package phpanalysis.controlflow
 
object ASTtoCFG {
  import parser.Trees._
  import analyzer.Symbols._
  import analyzer.Types._
  import CFGTrees._

  /** Builds a control flow graph from a method declaration. */
  def convertAST(statements: List[Statement]): CFG = {
    val cfg: CFG = new CFG
    val assertionsEnabled: Boolean = true
    type Vertex = cfg.Vertex

    /** Creates fresh variable names on demand. */
    object FreshName {
      var count = 0

      def apply(prefix: String): String = {
        val post = count
        count = count  + 1
        prefix + "#" + post
      }
    }

    /** Creates fresh variable tree nodes on demand. */
    object FreshVariable {
      def apply(prefix: String, tpe: Type) = CFGTempID(FreshName(prefix)).setType(tpe)
      def apply(prefix: String) = CFGTempID(FreshName(prefix)).setType(TMixed)
    }

    /** Helper to add edges and vertices to the nascent CFG while maintaining
     * the current "program counter", that is, the point from which the rest
     * of the graph should be built. */
    object Emit {
      private var pc: Vertex = cfg.entry
      def getPC: Vertex = pc
      def setPC(v: Vertex) = { pc = v }

      // emits a statement between two program points
      def statementBetween(from: Vertex, stat: CFGStatement, to : Vertex): Unit = {
        cfg += (from, stat, to)
      }
      
      // emits a statement from the current PC and sets the new PC after it
      def statement(stat: CFGStatement): Unit = {
        val v = cfg.newVertex
        cfg += (pc, stat, v)
        setPC(v)
      }

      // emits a statement from the current PC to an existing program point
      def statementCont(stat: CFGStatement, cont: Vertex) = {
        cfg += (pc, stat, cont)
      }

      // emits an ''empty'' statement (equivalent to unconditional branch) from the current PC to an existing point
      def goto(cont: Vertex) = {
        cfg += (pc, CFGSkip, cont)
      }
    }

    /** Generates the part of the graph corresponding to the branching on a conditional expression */
    def condExpr(ex: Expression, falseCont: Vertex, trueCont: Vertex): Unit = {
      // should have been enforced by type checking.
      // assert(ex.getType == TBoolean)

      ex match {
          case BooleanAnd(lhs, rhs) =>
            val soFarTrueV = cfg.newVertex
            condExpr(lhs, falseCont, soFarTrueV)
            Emit.setPC(soFarTrueV)
            condExpr(rhs, falseCont, trueCont)
          case BooleanOr(lhs, rhs) =>
            val soFarFalseV = cfg.newVertex
            condExpr(lhs, soFarFalseV, trueCont)
            Emit.setPC(soFarFalseV)
            condExpr(rhs, falseCont, trueCont)
          case Equal(lhs, rhs) =>
            val e1 = expr(lhs)
            val e2 = expr(rhs)
            Emit.statementCont(CFGAssume(e1, EQUALS, e2), trueCont)
            Emit.statementCont(CFGAssume(e1, NOTEQUALS, e2), falseCont)
          case Smaller(lhs, rhs) =>
            val e1 = expr(lhs)
            val e2 = expr(rhs)
            Emit.statementCont(CFGAssume(e1, LT, e2), trueCont)
            Emit.statementCont(CFGAssume(e1, GEQ, e2), falseCont)
          case mc @ MethodCall(obj, id, args) =>
            val e = expr(mc)
            Emit.statementCont(CFGAssume(e, EQUALS, CFGTrue), trueCont)
            Emit.statementCont(CFGAssume(e, NOTEQUALS, CFGTrue), falseCont)
          case PHPTrue() =>
            Emit.goto(trueCont)
          case PHPFalse() =>
            Emit.goto(falseCont)
          case BooleanNot(not) =>
            condExpr(not, trueCont, falseCont)
          case v @ SimpleVariable(name) => 
            val e = expr(v)
            Emit.statementCont(CFGAssume(e, EQUALS, CFGTrue), trueCont)
            Emit.statementCont(CFGAssume(e, NOTEQUALS, CFGTrue), falseCont)
          case _ => error("Woot!?! unexpected expression in condExpr()")
      }
    }
 
    /** Transforms an identifier from the AST to one for the CFG. */
    def idFromId(id: Identifier): CFGIdentifier = {
      // should be enforced by type checking and by construction
      assert(id.getSymbol.isInstanceOf[VariableSymbol])
      CFGIdentifier(id.getSymbol.asInstanceOf[VariableSymbol]).setPos(id)
    }
    
    /** If an expression can be translated without flattening, does it and
      * returns the result in a Some(...) instance. Otherwise returns None. */
    def alreadySimple(ex: Expression): Option[CFGSimpleValue] = ex match {
      case PHPInteger(v) => Some(CFGNumLit(v))
      case PHPString(v) => Some(CFGStringLit(v))
      case PHPTrue() => Some(CFGTrue)
      case PHPFalse() => Some(CFGFalse)
      case _ => None
    }
 
    /** Flattens complex expressions, or simply returns simple ones. */
    
    def notyet(ex: Expression) = throw new Exception("Not yet implemented in CFG: "+ex);

    def exprStore(v: CFGVariable, ex: Expression): CFGStatement = alreadySimple(ex) match {
        case Some(x) => CFGAssign(v, x)
        case None => 
            ex match {
                case ExpandArray(vars, expr) =>
                    notyet(ex)
                case Assign(vari, value, byref) =>
                    notyet(ex)
                case Clone(obj) =>
                    notyet(ex)
                case Plus(lhs, rhs) =>
                    notyet(ex)
                case Minus(lhs, rhs) =>
                    notyet(ex)
                case Div(lhs, rhs) =>
                    notyet(ex)
                case Mult(lhs, rhs) =>
                    notyet(ex)
                case Concat(lhs, rhs) =>
                    notyet(ex)
                case Mod(lhs, rhs) =>
                    notyet(ex)
                case PreInc(rhs) =>
                    notyet(ex)
                case PostInc(rhs) =>
                    notyet(ex)
                case PreDec(rhs) =>
                    notyet(ex)
                case PostDec(rhs) =>
                    notyet(ex)
                case BooleanAnd(lhs, rhs) =>
                    notyet(ex)
                case BooleanOr(lhs, rhs) =>
                    notyet(ex)
                case BooleanXor(lhs, rhs) =>
                    notyet(ex)
                case BitwiseAnd(lhs, rhs) =>
                    notyet(ex)
                case BitwiseOr(lhs, rhs) =>
                    notyet(ex)
                case BitwiseXor(lhs, rhs) =>
                    notyet(ex)
                case ShiftLeft(lhs, rhs) =>
                    notyet(ex)
                case ShiftRight(lhs, rhs) =>
                    notyet(ex)
                case BooleanNot(rhs) =>
                    notyet(ex)
                case BitwiseNot(rhs) =>
                    notyet(ex)
                case Equal(lhs, rhs) =>
                    notyet(ex)
                case Identical(lhs, rhs) =>
                    notyet(ex)
                case Smaller(lhs, rhs) =>
                    notyet(ex)
                case SmallerEqual(lhs, rhs) =>
                    notyet(ex)
                case InstanceOf(lhs, rhs) =>
                    notyet(ex)
                case Ternary(cond, then, elze) =>
                    notyet(ex)
                case Cast(typ, value) =>
                    notyet(ex)
                case Silence(value) =>
                    notyet(ex)
                case Exit(value) =>
                    notyet(ex)
                case Array(values) =>
                    notyet(ex)
                case Execute(value) =>
                    notyet(ex)
                case Print(value) =>
                    notyet(ex)
                case Eval(value) =>
                    notyet(ex)
                case Closure(args, retref, body) =>
                    notyet(ex)
                case Isset(vs) =>
                    notyet(ex)
                case Empty(v) =>
                    notyet(ex)
                case Include(path, once) =>
                    notyet(ex)
                case Require(path, once) =>
                    notyet(ex)
                case Constant(name) =>
                    notyet(ex)
                case ClassConstant(cl, const) =>
                    notyet(ex)
                case New(cl, args) =>
                    notyet(ex)
                case FunctionCall(name, args) =>
                    notyet(ex)
                case MethodCall(obj, name, args) =>
                    notyet(ex)
                case StaticMethodCall(cl, name, args) =>
                    notyet(ex)
              /*
              case Plus(lhs: Expression, rhs: Expression) => 
                CFGAssignBinary(v, expr(lhs), PLUS, expr(rhs))
              case Minus(lhs: Expression, rhs: Expression) => 
                CFGAssignBinary(v, expr(lhs), MINUS, expr(rhs))
              case Div(lhs: Expression, rhs: Expression) => 
                CFGAssignBinary(v, expr(lhs), DIV, expr(rhs))
              case Mult(lhs: Expression, rhs: Expression) => 
                CFGAssignBinary(v, expr(lhs), TIMES, expr(rhs))
              case ArrayOffset(arr: Expression, index: Expression) => 
                if (assertionsEnabled) {
                    arrayBoundsAssert(arr, index)
                }
                CFGAssignBinary(v, expr(arr), ARRAYREAD, expr(index)) 
              case NewIntArray(size: Expression) => 
                CFGAssignUnary(v, NEWINTARRAY, expr(size))
              case Not(e: Expression) => 
                CFGAssignUnary(v, NOT, expr(e))
              case MethodCall(obj, id, args) => 
                CFGAssignMethodCall(v, expr(obj), id, args.map(expr))
              case _ => 
                CFGAssign(v, expr(ex))
                */
            }
    }

    def expr(ex: Expression): CFGSimpleValue = alreadySimple(ex) match {
        case Some(x) => x
        case None => 
            ex match {
                case _ => 
                    val v = FreshVariable("expr")
                    ex match {
                        case BooleanAnd(lhs, rhs) =>
                            val trueV = cfg.newVertex
                            val falseV = cfg.newVertex
                            condExpr(ex, falseV, trueV)
                            val afterV = cfg.newVertex
                            Emit.statementBetween(falseV, CFGAssign(v, CFGFalse), afterV)
                            Emit.statementBetween(trueV, CFGAssign(v, CFGTrue), afterV)
                            Emit.setPC(afterV)
                        case BooleanOr(lhs, rhs) => 
                            val trueV = cfg.newVertex
                            val falseV = cfg.newVertex
                            condExpr(ex, falseV, trueV)
                            val afterV = cfg.newVertex
                            Emit.statementBetween(falseV, CFGAssign(v, CFGFalse), afterV)
                            Emit.statementBetween(trueV, CFGAssign(v, CFGTrue), afterV)
                            Emit.setPC(afterV)
                        case Equal(lhs, rhs) =>
                            val trueV = cfg.newVertex
                            val falseV = cfg.newVertex
                            condExpr(ex, falseV, trueV)
                            val afterV = cfg.newVertex
                            Emit.statementBetween(falseV, CFGAssign(v, CFGFalse), afterV)
                            Emit.statementBetween(trueV, CFGAssign(v, CFGTrue), afterV)
                            Emit.setPC(afterV)
                        case Smaller(lhs, rhs) => 
                            val trueV = cfg.newVertex
                            val falseV = cfg.newVertex
                            condExpr(ex, falseV, trueV)
                            val afterV = cfg.newVertex
                            Emit.statementBetween(falseV, CFGAssign(v, CFGFalse), afterV)
                            Emit.statementBetween(trueV, CFGAssign(v, CFGTrue), afterV)
                            Emit.setPC(afterV)
                        case Plus(lhs, rhs) => Emit.statement(CFGAssignBinary(v, expr(lhs), PLUS, expr(rhs)))
                            /*(lhs.getType, rhs.getType) match {
                            case (TInt, TInt) => CFGAssignBinary(v, expr(lhs), PLUS, expr(rhs))
                            case (TString, _) | (_, TString) => CFGAssignBinary(v, expr(lhs), PLUS, expr(rhs))
                        }*/
                        case Minus(lhs, rhs) =>
                            Emit.statement(CFGAssignBinary(v, expr(lhs), MINUS, expr(rhs)))
                        case Div(lhs, rhs) =>
                            Emit.statement(CFGAssignBinary(v, expr(lhs), DIV, expr(rhs)))
                        case Mult(lhs, rhs) =>
                            Emit.statement(CFGAssignBinary(v, expr(lhs), MULT, expr(rhs)))
                        case ArrayEntry(arr, index) => 
                            Emit.statement(CFGAssignBinary(v, expr(arr), ARRAYREAD, expr(index))) 
                        case MethodCall(obj, StaticMethodRef(id), args) => 
                            Emit.statement(CFGAssignMethodCall(v, expr(obj), id, args.map {a => expr(a.value) }))
                        case BooleanNot(e) => 
                            Emit.statement(CFGAssignUnary(v, BOOLEANNOT, expr(e)))
                        case _ => error("Woot!?! Unexpected exprression in expr()"+ ex.getClass)
                    }
                    v
            }
    }

    /** Emits a sequence of statements. */
    def stmts(sts: List[Statement], cont: Vertex): Unit = sts match {
        case s::s2::sr => 
            val tmp = cfg.newVertex
            stmt(s, tmp)
            stmts(s2::sr, cont)
        case s::Nil => 
            stmt(s, cont)
        case Nil => 
    }
    
    /** Emits a single statement. cont = where to continue after the statement */
    def stmt(s: Statement, cont: Vertex): Unit = s match {
        case Block(sts) =>
            stmts(sts, cont)
            Emit.setPC(cont)
        case If(cond, then, elze) =>
            val thenV = cfg.newVertex
            val elzeV = cfg.newVertex
            cfg.openGroup("if", Emit.getPC)
            elze match {
                case Some(st) =>
                    condExpr(cond, elzeV, thenV)
                    Emit.setPC(elzeV)
                    stmt(st, cont);
                case None =>
                    condExpr(cond, cont, thenV)
            }
            Emit.setPC(thenV)
            stmt(then, cont)
            cfg.closeGroup(cont)
        case While(cond, st) =>
            val beginV = Emit.getPC
            val beginWhileV = cfg.newVertex
            cfg.openGroup("while", Emit.getPC)
            condExpr(cond, cont, beginWhileV)
            Emit.setPC(beginWhileV)
            stmt(st, beginV)
            cfg.closeGroup(cont)
            Emit.setPC(cont)
        case Assign(SimpleVariable(id), value, byref) =>
            Emit.statementCont(exprStore(idFromId(id), value), cont)
            Emit.setPC(cont)
        /*
        case ArrayAssign(id, index, value) =>
            if (assertionsEnabled) {
                arrayBoundsAssert(id, index)
            }
            Emit.statementCont(CFGArrayAssign(idFromId(id), expr(index), expr(value)), cont)
            Emit.setPC(cont)
        */
    } 

    /** Removes useless Skip edges by short-circuiting them. */
    def fewerSkips = {
      for (v <- cfg.V) {
        if ((v != cfg.entry) &&
              (v != cfg.exit) &&
              (v.out.size == 1)) {
          for (eOut <- v.out) {
            if (eOut.lab == CFGSkip) {
              for (eIn <- v.in) {
                // remove old edge
                cfg -= (eIn.v1, eIn.lab, eIn.v2)
                cfg -= (eOut.v1, eOut.lab, eOut.v2)
                // insert new edge with label of incoming one
                cfg += (eIn.v1, eIn.lab, eOut.v2)
              }
            }
          }
        }
      }
    }

    val retvertex = cfg.newVertex
    val retV = FreshVariable("result")

    Emit.setPC(cfg.entry)
    stmts(statements, retvertex)
    Emit.setPC(cfg.exit)
    //fewerSkips
    cfg
    }
}
