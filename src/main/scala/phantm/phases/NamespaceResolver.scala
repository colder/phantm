package phantm.phases;

import phantm.Settings
import phantm.cfg.ControlFlowGraph
import phantm.ast.Trees._
import phantm.ast._

object NamespaceResolverPhase extends Phase {

    def name = "namespaces"
    def description = "Resolving namespaces"

    def run(ctx: PhasesContext): PhasesContext = {

      var ast = ctx.oast.get
      
      // First we resolve NamespaceStart into Namespaced
      ast = new NSWrapper(ast).transform

      // We collect all function/constant definitions
      var constants = Set[NSIdentifier]()
      var functions = Set[NSIdentifier]()
      for (stmt <- ast.stmts) stmt match {
        case ConstantDecl(nsid, _) => constants += nsid
        case FunctionDecl(nsid, _, _, _) => functions += nsid
        case Namespaced(ns, stmts) =>
          for (stmt <- stmts) stmt match {
            case ConstantDecl(nsid, _)       => constants += nsid defWithin ns
            case FunctionDecl(nsid, _, _, _) => functions += nsid defWithin ns
            case _ =>
          }
        case _ =>
      }

      ast = new NSResolver(ast, constants, functions).transform

      ctx.copy(oast = Some(ast))
    }
}

class NSWrapper(ast: Program) extends ASTTransform(ast) {

    override def trStmts(sts: List[Statement]): List[Statement] = {
      val stmts = super.trStmts(sts)

      var result = List[Statement]();
      var lastNS: Option[NamespaceStart] = None
      var soFar = List[Statement]();

      def appendSoFar() {
        lastNS match {
          case Some(ns) =>
            result = result ::: List(Namespaced(ns.name, soFar.reverse))
          case None =>
            result = result ::: (soFar.reverse)
        }
      }

      for (s <- stmts) {
        s match {
          case ns: NamespaceStart =>
            appendSoFar()
            lastNS = Some(ns)
            soFar = Nil
          case _ =>
            soFar = s :: soFar
        }
      }
      appendSoFar()

      result
    }

    override def trStmt(st: Statement): Statement = st match {
        case n @ Namespaced(id, body) =>
          n.copy(body = trStmts(body)) 
        case st => super.trStmt(st)

    }
}

class NSResolver(ast: Program, constants: Set[NSIdentifier], functions: Set[NSIdentifier]) extends ASTTransform(ast) {
    var importRules = Map[String, List[String]]()
    var nsContext = NSIdentifier(NSResolved, Nil)

    def resolveNSId(nsid: NSIdentifier) = {
      val r = nsid match {
        case NSIdentifier(NSNone, x :: y :: xs) if importRules contains x =>
          NSIdentifier(NSResolved, importRules(x) ::: y :: xs)
        case NSIdentifier(NSNone, parts) =>
          NSIdentifier(NSResolved, nsContext.parts ::: parts)
        case NSIdentifier(NSCurrent, parts) =>
          NSIdentifier(NSResolved, nsContext.parts ::: parts)
        case _ =>
          NSIdentifier(NSResolved, nsid.parts)
      }
      r.setPos(nsid).annotateFromC(nsid)
    }

    def resolveConstant(nsid: NSIdentifier) = {
      val conInNS = nsid defWithin nsContext

      val r = nsid match {
        case NSIdentifier(NSNone, x :: Nil) if constants contains conInNS =>
          conInNS
        case NSIdentifier(NSNone, x :: Nil) =>
          NSIdentifier(NSResolved, List(x))
        case _ =>
          resolveNSId(nsid)
      }

      r.setPos(nsid).annotateFromC(nsid)
    }

    def resolveFunction(nsid: NSIdentifier) = {
      val funInNS = nsid defWithin nsContext

      val r = nsid match {
        case NSIdentifier(NSNone, x :: Nil) if functions contains funInNS =>
          funInNS
        case NSIdentifier(NSNone, x :: Nil) =>
          NSIdentifier(NSResolved, List(x))
        case _ =>
          resolveNSId(nsid)
      }

      r.setPos(nsid).annotateFromC(nsid)
    }

    def resolveClass(nsid: NSIdentifier) = nsid match {
      case NSIdentifier(NSNone, x :: xs) if importRules contains x =>
        NSIdentifier(NSResolved, importRules(x) ::: xs).setPos(nsid).annotateFromC(nsid)
      case _ =>
        resolveNSId(nsid)
    }

    override def trStaticClassRef(scr: StaticClassRef): StaticClassRef = {
      StaticClassRef(resolveClass(scr.name)).setPos(scr).annotateFromC(scr)
    }

    override def trFuncRef(fr: FunctionRef): FunctionRef = (fr match {
        case VarFunctionRef(v) =>
          VarFunctionRef(trVariable(v))
        case DynamicFunctionRef(ex) =>
          DynamicFunctionRef(trExpr(ex))
        case StaticFunctionRef(name) =>
          StaticFunctionRef(resolveFunction(name))
    }).setPos(fr).annotateFromC(fr)

    override def trStmt(st: Statement): Statement = {
      var r = st match {
        case n @ Namespaced(id, body) =>
          val lastContext = nsContext
          nsContext = id

          val res = Block(trStmts(body)).setPos(n)

          nsContext = lastContext
          res

        case Import(from, as) =>
          importRules += as -> from.parts
          Void()

        case FunctionDecl(name, args, retref, body) =>
          FunctionDecl(name defWithin nsContext,
                       args map trArgDecl,
                       retref,
                       trStmt(body))
        case InterfaceDecl(name, interfaces, methods, consts) =>
          InterfaceDecl(name defWithin nsContext,
                        interfaces map trStaticClassRef,
                        methods map trMethod,
                        consts map trClassConst)

        case ClassDecl(name, flags, parent, interfaces, methods, static_props, props, consts) =>
           ClassDecl(name defWithin nsContext,
                    flags,
                    parent map trStaticClassRef,
                    interfaces map trStaticClassRef,
                    methods map trMethod,
                    static_props map trProperty,
                    props map trProperty,
                    consts map trClassConst)
        case ConstantDecl(name, value) =>
          ConstantDecl(name defWithin nsContext, trExpr(value))

        case _ =>
          super.trStmt(st)
      }

      r.setPos(st).annotateFromC(st)
    }

    override def trExpr(e: Expression): Expression = {
      val r = e match {
        case Constant(name) =>
          Constant(resolveConstant(name))
        case _ =>
          super.trExpr(e)
      }

      r.setPos(e).annotateFromC(e)
    }
}
