package phantm.ast
import Trees._

abstract class ASTTransform(p: Program) {
  def transform: Program = {
    Program(trStmts(p.stmts)).setPos(p)
  }

  def trStmts(stmts: List[Statement]): List[Statement] = stmts match {
    case st :: sts => trStmt(st) :: trStmts(sts)
    case Nil => Nil
  }

  def trIdentifier(id: Identifier) = id

  def trVariable(v: Variable): Variable = {
    val r = v match {
      case SimpleVariable(name) =>
        SimpleVariable(trIdentifier(name))
      case VariableVariable(name) =>
        VariableVariable(trExpr(name))
      case ArrayEntry(array, index) =>
        ArrayEntry(trExpr(array), trExpr(index))
      case NextArrayEntry(array) =>
        NextArrayEntry(trExpr(array))
      case ObjectProperty(obj, property) =>
        ObjectProperty(trExpr(obj), trIdentifier(property))
      case DynamicObjectProperty(obj, property) =>
        DynamicObjectProperty(trExpr(obj), trExpr(property))
      case ClassProperty(cl, property) =>
        ClassProperty(trClassRef(cl), trVariable(property))
      case ListVar(vars: List[Option[Variable]]) =>
        ListVar(vars.map(_.map(trVariable)))
    }

    r.setPos(v).annotateFromC(v)
  }

  def trScalar(s: Scalar): Scalar = {
    s
  }

  def trExpr(ex: Expression): Expression = {
    val r = ex match {
      case Alternatives(blocks: List[Expression]) =>
        Alternatives(blocks map trExpr)
      case Block(stmts) =>
        Block(stmts map trStmt)
      case v: Variable =>
        trVariable(v)
      case ExpandArray(vars, expr) =>
        ExpandArray(vars.map(_.map(trVariable)), trExpr(expr))
      case Assign(vari, value, byref) =>
        Assign(trVariable(vari), trExpr(value), byref)
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
      case PreInc(rhs) =>
        PreInc(trVariable(rhs))
      case PostInc(rhs) =>
        PostInc(trVariable(rhs))
      case PreDec(rhs) =>
        PreDec(trVariable(rhs))
      case PostDec(rhs) =>
        PostDec(trVariable(rhs))
      case Equal(lhs, rhs) =>
        Equal(trExpr(lhs), trExpr(rhs))
      case Identical(lhs, rhs) =>
        Identical(trExpr(lhs), trExpr(rhs))
      case Smaller(lhs, rhs) =>
        Smaller(trExpr(lhs), trExpr(rhs))
      case SmallerEqual(lhs, rhs) =>
        SmallerEqual(trExpr(lhs), trExpr(rhs))
      case InstanceOf(lhs, rhs) =>
        InstanceOf(trExpr(lhs), trClassRef(rhs))
      case Ternary(cond, then, elze) =>
        Ternary(trExpr(cond), then map trExpr, trExpr(elze))
      case Cast(typ, value) =>
        Cast(typ, trExpr(value))
      case Silence(value) =>
        Silence(trExpr(value))
      case Exit(value) =>
        Exit(value map trExpr)
      case Array(values) =>
        Array(values map { case (oe, e, b) => (oe map trExpr, trExpr(e), b) })
      case Execute(str) =>
        Execute(str)
      case Print(value) =>
        Print(trExpr(value))
      case Eval(value) =>
        Eval(trExpr(value))
      case Closure(args, imports, retref, body) =>
        Closure(args map trArgDecl, imports map trArgDecl, retref, trStmt(body))    
      case Isset(vs) =>
        Isset(vs map trVariable)
      case Empty(v) =>
        Empty(trVariable(v))
      case Include(path, once) =>
        Include(trExpr(path), once)
      case Require(path, once) =>
        Require(trExpr(path), once)
      case Constant(name) =>
        Constant(trNSId(name))
      case ClassConstant(cl, const) =>
        ClassConstant(trClassRef(cl), const)
      case New(cl, args) =>
        New(trClassRef(cl), args map trCallArg)
      case FunctionCall(name, args) =>
        FunctionCall(trFuncRef(name), args map trCallArg)
      case MethodCall(obj, name, args) =>
        MethodCall(trExpr(obj), trMethodRef(name), args map trCallArg)
      case StaticMethodCall(cl, name, args) =>
        StaticMethodCall(trClassRef(cl), trMethodRef(name), args map trCallArg)
      case ve: VoidExpr =>
        ve
      case s: Scalar =>
        trScalar(s)
    }

    r.setPos(ex).annotateFromC(ex)
  }

  def trArgDecl(ad: ArgumentDecl): ArgumentDecl = {
    ArgumentDecl(ad.v.copy(name = trIdentifier(ad.v.name)), ad.hint map trHint, ad.default map trExpr, ad.byref).setPos(ad).annotateFromC(ad)
  }

  def trMethod(md: MethodDecl): MethodDecl = {
    MethodDecl(trIdentifier(md.name), md.flags, md.args map trArgDecl, md.retref, md.body map trStmt).setPos(md).annotateFromC(md)
  }

  def trProperty(pd: PropertyDecl): PropertyDecl = {
    PropertyDecl(trIdentifier(pd.v), pd.flags, pd.default map trExpr).setPos(pd).annotateFromC(pd)
  }

  def trClassConst(cl: ClassConstantDecl): ClassConstantDecl = {
    ClassConstantDecl(trIdentifier(cl.v),trExpr(cl.value)).setPos(cl).annotateFromC(cl)
  }

  def trNSId(nsid: NSIdentifier): NSIdentifier = {
    nsid
  }

  def trCallArg(ca: CallArg): CallArg = {
    ca.copy(value = trExpr(ca.value))
  }

  def trFuncRef(fr: FunctionRef): FunctionRef = (fr match {
      case VarFunctionRef(v) =>
        VarFunctionRef(trVariable(v))
      case DynamicFunctionRef(ex) =>
        DynamicFunctionRef(trExpr(ex))
      case StaticFunctionRef(name) =>
        StaticFunctionRef(trNSId(name))
  }).setPos(fr).annotateFromC(fr)

  def trClassRef(cr: ClassRef): ClassRef = (cr match {
    case VarClassRef(v) =>
      VarClassRef(trVariable(v))
    case DynamicClassRef(ex) =>
      DynamicClassRef(trExpr(ex))
    case cr: StaticClassRef =>
      trStaticClassRef(cr)
    case cc: CalledClass =>
      CalledClass()
  }).setPos(cr).annotateFromC(cr)

  def trHint(th: TypeHint): TypeHint = th match {
    case THArray =>
      THArray
    case THObject(cl: ClassRef) =>
      THObject(trClassRef(cl))
  }

  def trMethodRef(mr: MethodRef): MethodRef = (mr match {
      case DynamicMethodRef(ex) =>
        DynamicMethodRef(trExpr(ex))
      case StaticMethodRef(id) =>
        StaticMethodRef(trIdentifier(id))
  }).setPos(mr).annotateFromC(mr)

  def trStaticClassRef(scr: StaticClassRef): StaticClassRef = {
    StaticClassRef(trNSId(scr.name)).setPos(scr).annotateFromC(scr)
  }

  def trStmt(st: Statement): Statement = (st match {
    case FunctionDecl(name, args, retref, body) =>
      FunctionDecl(trNSId(name),
                   args map trArgDecl,
                   retref,
                   trStmt(body))
    case NamespaceStart(nsid) =>
      NamespaceStart(trNSId(nsid))

    case Namespaced(name, body) =>
      Namespaced(trNSId(name), body map trStmt)

    case Import(from, as) =>
      Import(trNSId(from), as)

    case InterfaceDecl(name, interfaces, methods, consts) =>
      InterfaceDecl(trNSId(name),
                    interfaces map trStaticClassRef,
                    methods map trMethod,
                    consts map trClassConst)

    case ClassDecl(name, flags, parent, interfaces, methods, static_props, props, consts) =>
       ClassDecl(trNSId(name),
                flags,
                parent map trStaticClassRef,
                interfaces map trStaticClassRef,
                methods map trMethod,
                static_props map trProperty,
                props map trProperty,
                consts map trClassConst)
    case ConstantDecl(name, value) =>
      ConstantDecl(trNSId(name), trExpr(value))
    case Try(body, catches) =>
      Try(trStmt(body), catches map { c => Catch(c.cl, c.v, trStmt(c.body) )})
    case If(cond, then, elze) =>
      If(trExpr(cond), trStmt(then), elze map trStmt)
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
    case Global(vars) =>
      Global(vars map trVariable)
    case Static(initvars) =>
      Static(initvars map trInitVariable)
    case Throw(ex) =>
      Throw(trExpr(ex))
    case Unset(vars) =>
      Unset(vars map trVariable)
    case Goto(to) =>
      Goto(to)
    case Void() =>
      Void()
    case Html(content) =>
      Html(content)
    case LabelDecl(name) =>
      LabelDecl(trIdentifier(name))
    case e: Expression =>
      trExpr(e)
  }).setPos(st).annotateFromC(st)

  def trInitVariable(iv: InitVariable): InitVariable = {
    InitVariable(trVariable(iv.v), iv.init map trExpr).setPos(iv).annotateFromC(iv)
  }
}
