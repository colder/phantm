package phpanalysis.controlflow
 
object ASTToCFG {
  import parser.Trees._
  import analyzer.Symbols._
  import CFGTrees._

  /** Builds a control flow graph from a method declaration. */
  def convertAST(statements: List[Statement]): CFG = {
    // Contains the entry+exit vertices for continue/break
    var controlStack: List[(Vertex, Vertex)] = Nil
    var dispatchers: List[(Vertex, CFGTempID)] = Nil

    val cfg: CFG = new CFG
    val assertionsEnabled: Boolean = true
    val retval: CFGTempID = CFGTempID("retval");
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
      //def apply(prefix: String, tpe: Type) = CFGTempID(FreshName(prefix))
      def apply(prefix: String) = CFGTempID(FreshName(prefix))
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
          case Exit(Some(value)) =>
            val retV = FreshVariable("exit").setPos(ex)
            Emit.statementCont(exprStore(retV, value), cfg.exit)
          case Exit(None) =>
            val retV = FreshVariable("exit").setPos(ex)
            Emit.statementCont(CFGAssign(retV, CFGLong(0)).setPos(ex), cfg.exit)
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
            Emit.statementCont(CFGAssume(e1, EQUALS, e2).setPos(ex), trueCont)
            Emit.statementCont(CFGAssume(e1, NOTEQUALS, e2).setPos(ex), falseCont)
          case Identical(lhs, rhs) =>
            val e1 = expr(lhs)
            val e2 = expr(rhs)
            Emit.statementCont(CFGAssume(e1, IDENTICAL, e2).setPos(ex), trueCont)
            Emit.statementCont(CFGAssume(e1, NOTIDENTICAL, e2).setPos(ex), falseCont)
          case Smaller(lhs, rhs) =>
            val e1 = expr(lhs)
            val e2 = expr(rhs)
            Emit.statementCont(CFGAssume(e1, LT, e2).setPos(ex), trueCont)
            Emit.statementCont(CFGAssume(e1, GEQ, e2).setPos(ex), falseCont)
          case SmallerEqual(lhs, rhs) =>
            val e1 = expr(lhs)
            val e2 = expr(rhs)
            Emit.statementCont(CFGAssume(e1, LEQ, e2).setPos(ex), trueCont)
            Emit.statementCont(CFGAssume(e1, GT, e2).setPos(ex), falseCont)
          case PHPTrue() =>
            Emit.goto(trueCont)
          case PHPFalse() =>
            Emit.goto(falseCont)
          case PHPNull() =>
            Emit.goto(falseCont)
          case PHPInteger(n) =>
            if (n == 0)
                Emit.goto(falseCont)
            else
                Emit.goto(trueCont)
          case BooleanNot(not) =>
            condExpr(not, trueCont, falseCont)
          case _ =>
            val e = expr(ex)
            Emit.statementCont(CFGAssume(CFGTrue().setPos(ex), EQUALS, e).setPos(ex), trueCont)
            Emit.statementCont(CFGAssume(CFGTrue().setPos(ex), NOTEQUALS, e).setPos(ex), falseCont)
      }
    }
 
    /** Transforms a variable from the AST to one for the CFG. */
    def varFromVar(v: Variable): CFGVariable = v match {
        case SimpleVariable(id) => idFromId(id)
        case VariableVariable(ex) => CFGVariableVar(expr(ex)).setPos(v)
        case ArrayEntry(array, index) => CFGArrayEntry(expr(array), expr(index)).setPos(v)
        case NextArrayEntry(array) => CFGNextArrayEntry(expr(array)).setPos(v)
        case ObjectProperty(obj, property) => CFGObjectProperty(expr(obj), CFGString(property.value)).setPos(v)
        case DynamicObjectProperty(obj, property) => CFGObjectProperty(expr(obj), expr(property)).setPos(v)
        case ClassProperty(cl, property) => CFGClassProperty(cl, expr(property)).setPos(v)
    }

    /** Transforms an identifier from the AST to one for the CFG. */
    def idFromId(id: Identifier): CFGIdentifier = {
        // should be enforced by type checking and by construction
        id.getSymbol match {
            case vs: VariableSymbol =>
                CFGIdentifier(vs).setPos(id)
            case _ => error("Woooot?");
        }
    }
    
    /** If an expression can be translated without flattening, does it and
      * returns the result in a Some(...) instance. Otherwise returns None. */
    def alreadySimple(ex: Expression): Option[CFGSimpleValue] = ex match {
      case v: Variable =>
        Some(varFromVar(v))
      case Constant(c) =>
        Some(CFGConstant(c).setPos(ex))
      case ClassConstant(c, i) =>
        Some(CFGClassConstant(c, i).setPos(ex))
      case PHPInteger(v) =>
        Some(CFGLong(v).setPos(ex))
      case PHPFloat(v) =>
        Some(CFGFloat(v).setPos(ex))
      case PHPString(v) =>
        Some(CFGString(v).setPos(ex))
      case PHPTrue() =>
        Some(CFGTrue().setPos(ex))
      case PHPFalse() =>
        Some(CFGFalse().setPos(ex))
      case PHPNull() =>
        Some(CFGNull().setPos(ex))
      case _ => None
    }
 
    def notyet(ex: Expression) = throw new Exception("Not yet implemented in CFG: "+ex+"("+ex.getPos+")");

    // If the assignation can easily be done, do it already
    def exprStore(v: CFGVariable, ex: Expression): CFGStatement = exprStoreGet(v, ex) match {
        case Some(stmt) => stmt.setPos(ex)
        case None => CFGAssign(v, expr(ex)).setPos(ex);
    }

    def exprStoreGet(v: CFGVariable, ex: Expression): Option[CFGStatement] = alreadySimple(ex) match {
        case Some(x) => Some(CFGAssign(v, x))
        case None =>
            ex match {
                case Clone(obj) =>
                    Some(CFGAssign(v, CFGClone(expr(obj)).setPos(ex)))
                case Plus(lhs, rhs) =>
                    Some(CFGAssignBinary(v, expr(lhs), PLUS, expr(rhs)))
                case Minus(lhs, rhs) =>
                    Some(CFGAssignBinary(v, expr(lhs), MINUS, expr(rhs)))
                case Div(lhs, rhs) =>
                    Some(CFGAssignBinary(v, expr(lhs), DIV, expr(rhs)))
                case Mult(lhs, rhs) =>
                    Some(CFGAssignBinary(v, expr(lhs), MULT, expr(rhs)))
                case Concat(lhs, rhs) =>
                    Some(CFGAssignBinary(v, expr(lhs), CONCAT, expr(rhs)))
                case Mod(lhs, rhs) =>
                    Some(CFGAssignBinary(v, expr(lhs), MOD, expr(rhs)))
                case BooleanXor(lhs, rhs) =>
                    Some(CFGAssignBinary(v, expr(lhs), BOOLEANXOR, expr(rhs)))
                case BitwiseAnd(lhs, rhs) =>
                    Some(CFGAssignBinary(v, expr(lhs), BITWISEAND, expr(rhs)))
                case BitwiseOr(lhs, rhs) =>
                    Some(CFGAssignBinary(v, expr(lhs), BITWISEOR, expr(rhs)))
                case BitwiseXor(lhs, rhs) =>
                    Some(CFGAssignBinary(v, expr(lhs), BITWISEXOR, expr(rhs)))
                case ShiftLeft(lhs, rhs) =>
                    Some(CFGAssignBinary(v, expr(lhs), SHIFTLEFT, expr(rhs)))
                case ShiftRight(lhs, rhs) =>
                    Some(CFGAssignBinary(v, expr(lhs), SHIFTRIGHT, expr(rhs)))
                case BooleanNot(rhs) =>
                    Some(CFGAssignUnary(v, BOOLEANNOT, expr(rhs)))
                case BitwiseNot(rhs) =>
                    Some(CFGAssignUnary(v, BITSIWENOT, expr(rhs)))
                case InstanceOf(lhs, cr) =>
                    Some(CFGAssign(v, CFGInstanceof(expr(lhs), cr).setPos(ex)))
                case Ternary(cond, Some(then), elze) =>
                    Some(CFGAssign(v, CFGTernary(expr(cond), expr(then), expr(elze)).setPos(ex)))
                case Ternary(cond, None, elze) =>
                    Some(CFGAssign(v, CFGTernary(expr(cond), v, expr(elze)).setPos(ex)))
                case Silence(value) =>
                    Some(CFGAssign(v, expr(value)))
                case Execute(value) =>
                    Some(CFGAssign(v, CFGFunctionCall(internalFunction("shell_exec").setPos(ex), List(CFGString(value).setPos(ex))).setPos(ex)))
                case Print(value) =>
                    Some(CFGAssign(v, CFGFunctionCall(internalFunction("print").setPos(ex), List(expr(value))).setPos(ex)))
                case Eval(value) =>
                    Some(CFGAssign(v, CFGFunctionCall(internalFunction("eval").setPos(ex), List(expr(value))).setPos(ex)))
                case Empty(va) =>
                    Some(CFGAssign(v, CFGFunctionCall(internalFunction("empty").setPos(ex), List(expr(va))).setPos(ex)))
                case FunctionCall(StaticFunctionRef(_, _, name), args) =>
                    Some(CFGAssign(v, CFGFunctionCall(name, args map { a => expr(a.value) }).setPos(ex)))
                case fc @ FunctionCall(_, args) =>
                    Reporter.notice("Dynamic function call ignored", fc)
                    Some(CFGAssign(v, CFGAny().setPos(fc)))
                case MethodCall(obj, StaticMethodRef(id), args) => 
                    Some(CFGAssign(v, CFGMethodCall(expr(obj), id, args.map {a => expr(a.value) }).setPos(ex)))
                case StaticMethodCall(cl, StaticMethodRef(id), args) => 
                    Some(CFGAssign(v, CFGStaticMethodCall(cl, id, args.map {a => expr(a.value) }).setPos(ex)))
                case Array(Nil) =>
                    Some(CFGAssign(v, CFGEmptyArray()))
                case New(cr, args) =>
                    Some(CFGAssign(v, CFGNew(cr, args map { a => expr(a.value) })))
                case _ => 
                    None
            }
    }

    def internalFunction(name: String): parser.Trees.Identifier = {
        GlobalSymbols.lookupFunction(name) match {
            case Some(s) => Identifier(name).setSymbol(s)
            case None => Identifier(name);
        }
    }

    def expr(ex: Expression): CFGSimpleValue = alreadySimple(ex) match {
        case Some(x) => x
        case None => ex match {
            case _ =>
                var v: CFGVariable = FreshVariable("expr").setPos(ex)
                var retval: Option[CFGSimpleValue] = None
                exprStoreGet(v, ex) match {
                    case Some(stmt) => stmt.setPos(ex); Emit.statement(stmt)
                    case _ => ex match {
                        case ExpandArray(vars, e) =>
                            v = FreshVariable("array").setPos(ex);
                            Emit.statement(CFGAssign(v, expr(e)).setPos(ex));
                            for((vv, i) <- vars.zipWithIndex) {
                                vv match {
                                    case Some(vv) =>
                                        Emit.statement(CFGAssign(varFromVar(vv), CFGArrayEntry(v, CFGLong(i).setPos(vv)).setPos(vv)).setPos(ex))
                                    case None =>
                                }
                            }
                        case Assign(va, value, byref) =>
                            v = varFromVar(va);
                            Emit.statement(exprStore(v, value));
                        case PreInc(vAST) =>
                            val vCFG = varFromVar(vAST)
                            Emit.statement(CFGAssignBinary(vCFG, vCFG, PLUS, CFGLong(1)).setPos(ex))
                            v = vCFG;
                        case PreDec(vAST) =>
                            val vCFG = varFromVar(vAST)
                            Emit.statement(CFGAssignBinary(vCFG, vCFG, MINUS, CFGLong(1)).setPos(ex))
                            v = vCFG;
                        case PostInc(vAST) =>
                            val vCFG = varFromVar(vAST)
                            Emit.statement(CFGAssignBinary(v, vCFG, PLUS, CFGLong(1)).setPos(ex))
                            Emit.statement(CFGAssignBinary(vCFG, vCFG, PLUS, CFGLong(1)).setPos(ex))
                        case PostDec(vAST) =>
                            val vCFG = varFromVar(vAST)
                            Emit.statement(CFGAssignBinary(v, vCFG, MINUS, CFGLong(1)).setPos(ex))
                            Emit.statement(CFGAssignBinary(vCFG, vCFG, MINUS, CFGLong(1)).setPos(ex))
                        case _: BooleanAnd | _: BooleanOr | _: Equal | _: Identical | _: Smaller  | _: SmallerEqual =>
                            val trueV = cfg.newVertex
                            val falseV = cfg.newVertex
                            condExpr(ex, falseV, trueV)
                            val afterV = cfg.newVertex
                            Emit.statementBetween(falseV, CFGAssign(v, CFGFalse().setPos(ex)).setPos(ex), afterV)
                            Emit.statementBetween(trueV, CFGAssign(v, CFGTrue().setPos(ex)).setPos(ex), afterV)
                            Emit.setPC(afterV)
                        case Isset(vs) =>
                            if (vs.length > 1) {
                                notyet(ex); // TODO
                            } else {
                                Emit.statement(CFGAssign(v, CFGFunctionCall(internalFunction("isset"), List(expr(vs.first))).setPos(ex)).setPos(ex))
                            }
                        case Array(values) =>
                            Emit.statement(CFGAssign(v, CFGEmptyArray().setPos(ex)).setPos(ex))
                            var i = 0;
                            for (av <- values) av match {
                                case (Some(x), va, byref) =>
                                    Emit.statement(CFGAssign(CFGArrayEntry(v, expr(x)), expr(va)).setPos(ex))
                                case (None, va, byref) =>
                                    Emit.statement(CFGAssign(CFGArrayEntry(v, CFGLong(i).setPos(v)).setPos(va), expr(va)).setPos(ex))
                                    i += 1
                            }

                        case Constant(id) =>
                            Emit.statement(CFGAssign(v, CFGConstant(id).setPos(ex)).setPos(ex))

                        case ClassConstant(cl, id) =>
                            Emit.statement(CFGAssign(v, CFGClassConstant(cl, id).setPos(ex)).setPos(ex))

                        case Cast(typ, e) =>
                            Emit.statement(CFGAssign(v, CFGCast(typ, expr(e)).setPos(ex)).setPos(ex))

                        case Block(sts) =>
                            val endblock = cfg.newVertex
                            stmts(sts, endblock)
                            Emit.setPC(endblock)
                            Emit.statement(CFGAssign(v, CFGTrue()).setPos(ex))
                        case i: Include =>
                            // ignore
                            retval = Some(CFGFalse().setPos(i))
                        case r: Require =>
                            // ignore
                            retval = Some(CFGFalse().setPos(r))
                        case e: Exit =>
                            Emit.goto(cfg.exit)
                            Emit.setPC(cfg.newVertex)
                            retval = Some(new CFGNone().setPos(ex))

                        case _ => error("expr() not handling correctly: "+ ex +"("+ex.getPos+")")
                    }
                }
                retval match {
                    case Some(x) => x
                    case None => v
                }
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
    def stmt(s: Statement, cont: Vertex): Unit = { 
      s match {
        case LabelDecl(name) =>
            Emit.goto(cont)
        case Goto(_) =>
            Reporter.notice("Goto's are currently ignored", s)
            Emit.goto(cont)
        case Block(sts) =>
            stmts(sts, cont)
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
            controlStack = (beginV, cont) :: controlStack
            stmt(st, beginV)
            controlStack = controlStack.tail
            cfg.closeGroup(cont)
        case DoWhile(body, cond) =>
            val beginV = Emit.getPC
            val beginCheck = cfg.newVertex
            cfg.openGroup("doWhile", beginV)
            controlStack = (beginCheck, cont) :: controlStack
            stmt(body, beginCheck)
            condExpr(cond, cont, beginV)
            controlStack = controlStack.tail
            cfg.closeGroup(cont)
        case For(init, cond, step, then) => 
            cfg.openGroup("for", Emit.getPC)
            val beginCondV = cfg.newVertex
            val beginBodyV = cfg.newVertex
            val beginStepV = cfg.newVertex
            stmt(init, beginCondV);
            Emit.setPC(beginCondV);
            condExpr(cond, cont, beginBodyV)
            Emit.setPC(beginBodyV);
            controlStack = (beginStepV, cont) :: controlStack
            stmt(then, beginStepV);
            controlStack = controlStack.tail
            Emit.setPC(beginStepV);
            stmt(step, beginCondV);
            cfg.closeGroup(cont)
        case Throw(ex) =>
            Emit.goto(cont)
        case Try(body, catches) =>
            // For now, we execute the body and ignore the catches
            stmt(body, cont)
            /*
            val dispatchExceptionV = cfg.newVertex
            val ex = FreshVariable("exception")
            dispatchers = (dispatchExceptionV, ex) :: dispatchers



            // First, execute the body
            Emit.setPC(beginTryV)
            stmt(body)
            Emit.goto(cont)

            // We define the dispatcher based on the catch conditions
            val nextCatchV = cfg.newVertex
            val beginCatchV = cfg.newVertex
            for (c <- catches) c match {
                case Catch(cd, catchBody) =>
                    Emit.setPC(dispatchExceptionV)

                    // Connect the dispatcher to that catch
                    Emit.statementCont(CFGAssume(CFGInstanceof(ex, cd), EQUALS, CFGTrue), beginCatchV)
                    Emit.statementCont(CFGAssume(CFGInstanceof(ex, cd), NOTEQUALS, CFGTrue), nextCatchV)

                    Emit.setPC(beginCatchV)
                    stmt(catchBody)
                    Emit.goto(cont)

            }

            dispatchers = dispatchers.tail
            */
        case Switch(input, cases) =>
            val beginSwitchV = Emit.getPC
            var curCaseV = cfg.newVertex
            var nextCaseV = cfg.newVertex
            var nextCondV = cfg.newVertex
            var conds: List[(Expression, Vertex)] = Nil;
            var default: Option[Vertex] = None;

            cfg.openGroup("switch", beginSwitchV)

            controlStack = (cont, cont) :: controlStack

            Emit.setPC(curCaseV);
            // First, we put the statements in place:
            for (val c <- cases) c match {
                case (None, ts) =>
                    default = Some(Emit.getPC)

                    Emit.setPC(curCaseV)
                    stmt(ts, nextCaseV)
                    curCaseV = nextCaseV
                    nextCaseV = cfg.newVertex
                case (Some(v), Block(List())) =>
                    conds = conds ::: (v, Emit.getPC) :: Nil
                case (Some(v), Block(tss)) =>
                    conds = conds ::: (v, Emit.getPC) :: Nil

                    Emit.setPC(curCaseV)
                    stmts(tss, nextCaseV)
                    curCaseV = nextCaseV
                    nextCaseV = cfg.newVertex
            }


            // Then, we link to conditions
            Emit.setPC(beginSwitchV)
            for (val c <- conds) c match {
                case (ex, v) => 
                    condExpr(Equal(input, ex), nextCondV, v)
                    Emit.setPC(nextCondV)
                    nextCondV = cfg.newVertex
            }

            default match {
                case Some(v) =>
                    Emit.statementCont(CFGSkip, v);
                case None =>
                    Emit.statementCont(CFGSkip, cont);
            }

            Emit.setPC(curCaseV);
            Emit.statementCont(CFGSkip, cont);

            controlStack = controlStack.tail

            cfg.closeGroup(cont)
        case Continue(PHPInteger(level)) =>
            if (level > controlStack.length) {
                Reporter.error("Continue level exceeding control structure deepness.", s);
            } else {
                Emit.statementCont(CFGSkip.setPos(s), controlStack((level-1).toInt)._1)
            }
        case Continue(_) =>
            Reporter.notice("Dynamic continue statement ignored", s);
        case Break(PHPInteger(level)) =>
            if (level > controlStack.length) {
                Reporter.error("Break level exceeding control structure deepness.", s);
            } else {
                Emit.statementCont(CFGSkip.setPos(s), controlStack((level-1).toInt)._2)
            }
        case Break(_) =>
            Reporter.notice("Dynamic break statement ignored", s);
        case Static(vars) =>
            for (v <- vars) v match {
                case InitVariable(SimpleVariable(id), Some(ex)) =>
                    Emit.statementCont(exprStore(idFromId(id), ex), cont)
                    Emit.setPC(cont);
                case InitVariable(SimpleVariable(id), None) =>
                    Emit.statementCont(CFGAssign(idFromId(id), CFGNull().setPos(id)).setPos(s), cont)
                    Emit.setPC(cont);
                case _ => // ignore
                    Emit.goto(cont)
            }
        case Global(vars) =>
            Emit.goto(cont)
        case Echo(exs) =>
            var nextEcho = cfg.newVertex
            var lastValidNext = nextEcho
            for (e <- exs) {
                Emit.statementCont(CFGPrint(expr(e)).setPos(s), nextEcho)
                Emit.setPC(nextEcho)
                lastValidNext = nextEcho
                nextEcho = cfg.newVertex
            }

            Emit.setPC(lastValidNext)
            Emit.goto(cont)
        case Html(content) =>
            Emit.statementCont(CFGPrint(CFGString(content).setPos(s)).setPos(s), cont)
        case Void() =>
            Emit.goto(cont)
        case Unset(vars) =>
            var nextUnset = cfg.newVertex
            var lastValidNext = nextUnset
            for (v <- vars) {
                Emit.statementCont(CFGUnset(varFromVar(v)).setPos(s), nextUnset)
                Emit.setPC(nextUnset)
                lastValidNext = nextUnset
                nextUnset = cfg.newVertex
            }

            Emit.setPC(lastValidNext)
            Emit.goto(cont)
        case Return(expr) =>
            Emit.statementCont(exprStore(retval, expr), cfg.exit)
        case Assign(SimpleVariable(id), value, byref) =>
            Emit.statementCont(exprStore(idFromId(id), value), cont)
        case Foreach(ex, as, _, optkey, _, body) =>
            val v = FreshVariable("val").setPos(ex)
            val condV = cfg.newVertex
            val assignCurV = cfg.newVertex
            val assignKeyV = cfg.newVertex
            val bodyV = cfg.newVertex
            val nextV = cfg.newVertex
            Emit.statementCont(exprStore(v, ex), condV)

            Emit.setPC(condV)
            Emit.statementCont(CFGAssume(CFGTrue().setPos(s), EQUALS, CFGArrayCurIsValid(v)).setPos(s), assignCurV)
            Emit.statementCont(CFGAssume(CFGTrue().setPos(s), NOTEQUALS, CFGArrayCurIsValid(v)).setPos(s), cont)

            Emit.setPC(assignCurV)
            Emit.statementCont(CFGAssign(varFromVar(as), CFGArrayCurElement(v).setPos(s)).setPos(as), assignKeyV)

            Emit.setPC(assignKeyV)
            optkey match {
                case Some(k) =>
                    Emit.statementCont(CFGAssign(varFromVar(k).setPos(k), CFGArrayCurKey(v).setPos(s)).setPos(k), bodyV)
                case None =>
                    Emit.goto(bodyV)
            }
            Emit.setPC(bodyV)

            stmt(body, nextV)

            Emit.setPC(nextV)
            Emit.statementCont(CFGAssign(v, CFGArrayNext(v).setPos(s)).setPos(s), condV)
        case _: FunctionDecl | _: ClassDecl | _: InterfaceDecl => 
            /* ignore */
            Emit.goto(cont);
        case e: Exit =>
            Emit.goto(cfg.exit)
            Emit.setPC(cfg.newVertex)
        case e: Expression =>
            Emit.statementCont(expr(e), cont);
        case _ =>
            Reporter.notice("Not yet implemented (AST->CFG): "+s, s);
      }
      Emit.setPC(cont)
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

    Emit.setPC(cfg.entry)
    val codeEntry = cfg.newVertex
    Emit.statementBetween(cfg.entry, CFGAssign(retval, CFGNull()) , codeEntry)
    Emit.setPC(codeEntry)
    stmts(statements, cfg.exit)
    Emit.setPC(cfg.exit)
    fewerSkips
    cfg
    }
}
