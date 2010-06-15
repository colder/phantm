package phantm.phases

import phantm._
import phantm.util.{Reporter, Positional, Evaluator}
import phantm.ast.Trees._
import phantm.ast.ASTTraversal
import phantm.types._
import phantm.symbols._
import phantm.annotations.SourceAnnotations.{Parser => CommentParser}

import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap

object SymbolsCollectionPhase extends Phase {

    def name = "Symbols collections"
    def description = "Collecting symbols"

    def run(ctx: PhasesContext): PhasesContext = {
        CollectSymbols(ctx.oast.get) execute;
        ctx
    }

}

case class SymContext(varScope: Scope, cl: Option[ClassSymbol], iface: Option[IfaceSymbol]);

case class CollectSymbols(node: Tree) extends ASTTraversal[SymContext](node, SymContext(GlobalSymbols, None, None)) {
    var classCycleDetectionSet = new HashSet[ClassDecl]
    var classesToPass = List[ClassDecl]()
    var classList: List[(ClassSymbol, ClassDecl)] = Nil

    /**
     * Visit classes and add them to a waiting list, so they can be processed in right order
     */
    def visitClasses(node: Tree, ctx: SymContext): (SymContext, Boolean) = {
        node match {
            case cl @ ClassDecl(name, flags, parent, interfaces, methods, static_props, props, consts) =>
                classesToPass = classesToPass ::: List(cl)
            case id @ InterfaceDecl(name, parents, methods, consts) =>
                // TODO: actually safely register interfaces
                GlobalSymbols.registerIface(new IfaceSymbol(name.value, Nil))

            case _ =>
        }
        (ctx, true)
    }

    def firstClassPass : Unit = classesToPass match {
      case Nil =>
      case cd :: cds2 =>
          GlobalSymbols.lookupClass(cd.name.value) match {
            case None =>
              firstClassPass0(cd)
            case Some(x) =>
              Reporter.notice("Class " + x.name + " already declared (previously declared at "+x.getPos+")", cd)
          }
          classesToPass = cds2
          firstClassPass

    }

    def firstClassPass0(cd: ClassDecl): Unit = {
      if (classCycleDetectionSet.contains(cd)) { 
        Reporter.error("Classes " + classCycleDetectionSet.map(x => x.name.value).mkString(" -> ") + " form an inheritance cycle", cd)
        return;
      }

      classCycleDetectionSet += cd

      val p: Option[ClassSymbol] = cd.parent match {
        case Some(x) => GlobalSymbols.lookupClass(x.name.value) match {
          case None => {
            var foundParent = false
            for (c <- classesToPass if c.name.value.equals(x.name.value) && !foundParent) {
              firstClassPass0(c)
              foundParent = true
            }
              
            GlobalSymbols.lookupClass(x.name.value) match {
              case None => Reporter.error("Class " + cd.name.value + " extends non-existent class " + x.name.value, x); None
              case x => x
            }
          }
          case Some(pcs) => Some(pcs)
        }
        case None => None
      }
      val cs = new ClassSymbol(cd.name.value, p, Nil).setPos(cd);
      GlobalSymbols.registerClass(cs)

      classList = classList ::: List((cs,cd))
      classCycleDetectionSet -= cd
    }


    def secondClassPass(cd: ClassDecl, cs: ClassSymbol): Unit = {
        for (m <- cd.methods) {
            val ms = new MethodSymbol(cs, m.name.value, getVisibility(m.flags)).setPos(m)
            cs.registerMethod(ms)
            ms.registerPredefVariables
            for (a <- m.args) {
                var t = TypeHelpers.typeHintToType(a.hint)

                if (a.default == Some(PHPNull)) {
                    /*
                     * PHP Hack: if you pass null as default value, then null
                     * is also accepted as type
                     */
                     t = TUnion(t, TNull)
                }

                val as = new ArgumentSymbol(a.v.name.value, a.byref, a.default != None).setPos(a.v)
                as.typ = t
                ms.registerArgument(as);
            }

            ms.attachComment(m.comment);

            val t = if (m.comment != None) {

                if (Settings.get.inlineMode != InlineNone) {
                    ms.shouldInline = CommentParser.shouldInline(m.comment.get)
                }
                ms.isPure = CommentParser.isPure(m.comment.get)

                val (args, ret) = CommentParser.getFunctionTypes(m.comment.get)

                var foundOne = false

                val ftargs = for ((n, as) <- ms.argList) yield {
                    if (args contains n) {
                        (args(n), as.byref, as.optional)
                    } else {
                        (TAny, as.byref, as.optional)
                    }
                }

                TFunction(ftargs, ret)
            } else {
                TFunction(Nil, TAny)
            }


            val ftargs = for (((n, a), i) <- ms.argList.zipWithIndex) yield {
                if (t.args.size <= i) {
                    (a.typ, a.byref, a.optional)
                } else {
                    if (m.args(i).default != None) {
                        val tde = TypeHelpers.exprToType(m.args(i).default)
                        checkTypeHint(t.args(i)._1, tde, a)
                    }
                    val newT = checkTypeHint(t.args(i)._1, a.typ, a)
                    a.typ = newT
                    (newT, a.byref, a.optional)
                }
            }
            ms.registerFType(TFunction(ftargs, t.ret))
        }

        for (p <- cd.props) {
            val th = TypeHelpers.exprToType(p.default)

            val ps = new PropertySymbol(cs, p.v.value, getVisibility(p.flags)).setPos(p)

            val t = if (p.comment != None) {
                CommentParser.getVarType(p.comment.get).getOrElse(th)
            } else {
                th
            }

            ps.typ = defaultTypeWF(checkTypeHint(t, th, p))
            cs.registerProperty(ps)
        }

        for (p <- cd.static_props) {
            val th = TypeHelpers.exprToType(p.default)

            val ps = new PropertySymbol(cs, p.v.value, getVisibility(p.flags)).setPos(p)

            val t = if (p.comment != None) {
                CommentParser.getVarType(p.comment.get).getOrElse(th)
            } else {
                th
            }

            ps.typ = defaultTypeWF(checkTypeHint(t, th, p))


            cs.registerStaticProperty(ps)
        }

        for (c <- cd.consts) {
            val ccs = c.value match {
                case sc: Scalar => 
                    new ClassConstantSymbol(cs, c.v.value, Some(sc)).setPos(c)
                case _ =>
                    new ClassConstantSymbol(cs, c.v.value, None).setPos(c)
            }

            val th = TypeHelpers.exprToType(c.value)
            val t = if (c.comment != None) {
                CommentParser.getConstType(c.comment.get).getOrElse(th)
            } else {
                th
            }

            ccs.typ = checkTypeHint(t, th, c)

            cs.registerConstant(ccs)
        }
    }

    def checkTypeHint(annoType: Type, hintType: Type, pos: Positional): Type = {
        val res = TypeLattice.meet(annoType, hintType)

        if (res == TBottom) {
            var verbosity = 0

            val t = (annoType, hintType) match {
                case (_, TNull) =>
                    verbosity = 2
                    TypeLattice.join(annoType, hintType)
                case (t1, t2) if TypeLattice.leq(annoType, TFloat) && TypeLattice.leq(hintType, TInt) =>
                    verbosity = 3
                    TNumeric
                case _ =>
                    annoType

            }

            // Display error
            if (Settings.get.verbosity >= verbosity) {
                Reporter.notice("Annotation: "+annoType.toText(BaseTypeEnvironment)+" incompatible with type hint or default value: "+hintType.toText(BaseTypeEnvironment), pos)
            }

            t
        } else {
            annoType
        }
    }

    /**
     * Visit the nodes and aggregate information inside the context to provide
     * hints about obvious errors directly from the AST
     */
    def visit(node: Tree, ctx: SymContext): (SymContext, Boolean) = {
        var newCtx = ctx;
        var continue = true;

        node match {
            case fd @ FunctionDecl(name, args, retref, body) =>
                val fs = new FunctionSymbol(name.value).setPos(fd).setUserland
                for(a <- args) {
                    var t = TypeHelpers.typeHintToType(a.hint)

                    if (a.default == Some(PHPNull)) {
                        /*
                         * PHP Hack: if you pass null as default value, then null
                         * is also accepted as type
                         */
                         t = t union TNull
                    }

                    val as = new ArgumentSymbol(a.v.name.value, a.byref, a.default != None).setPos(a.v)
                    as.typ = t

                    fs.registerArgument(as);
                }

                val t = if (fd.comment != None) {
                    if (Settings.get.inlineMode != InlineNone) {
                        fs.shouldInline = CommentParser.shouldInline(fd.comment.get)
                    }

                    fs.isPure = CommentParser.isPure(fd.comment.get)

                    val (args, ret) = CommentParser.getFunctionTypes(fd.comment.get)

                    val ftargs = for ((n, as) <- fs.argList) yield {
                        if (args contains n) {
                            (args(n), as.byref, as.optional)
                        } else {
                            (TAny, as.byref, as.optional)
                        }
                    }

                    TFunction(ftargs, ret)
                } else {
                    // TODO: The prototypes should be checked
                    TFunction(Nil, TAny)
                }

                val ftargs = for (((n, a), i) <- fs.argList.zipWithIndex) yield {
                    if (t.args.size <= i) {
                        (a.typ, a.byref, a.optional)
                    } else {
                        if (args(i).default != None) {
                            val tde = TypeHelpers.exprToType(args(i).default)
                            checkTypeHint(t.args(i)._1, tde, a)
                        }
                        val newT = checkTypeHint(t.args(i)._1, a.typ, a)
                        a.typ = newT
                        (newT, a.byref, a.optional)
                    }
                }
                fs.registerFType(TFunction(ftargs, t.ret))

                fs.registerPredefVariables
                GlobalSymbols.registerFunction(fs)
                name.setSymbol(fs)
                newCtx = SymContext(fs, None, None)

            case ClassDecl(name, flags, parent, interfaces, methods, static_props, props, consts) =>
                GlobalSymbols.lookupClass(name.value) match {
                    case Some(cs) =>
                        name.setSymbol(cs);
                        newCtx = SymContext(ctx.varScope, Some(cs), None)
                    case None => error("Woops ?!? Came across a phantom class");
                }

            case InterfaceDecl(name, parents, methods, consts) =>
                GlobalSymbols.lookupIface(name.value) match {
                    case Some(iface) =>
                        newCtx = SymContext(ctx.varScope, None, Some(iface))
                    case None => error("Woops ?!? Came across a phantom interface");
                }

            case MethodDecl(name, flags, args, retref, body) =>
                (ctx.cl, ctx.iface) match {
                    case (Some(cs), _) => cs.lookupMethod(name.value, Some(cs)) match {
                        case LookupResult(Some(ms: MethodSymbol), _, _) =>
                            name.setSymbol(ms)
                            newCtx = SymContext(ms, Some(cs), None)
                        case _ => error("Woops?! No such method declared yet: "+cs.name+"::"+name.value+" ??")
                    }
                    case (None, Some(iface)) =>
                        // nothing
                    case (None, None) =>
                        error("Woops?!? Got into a method without any class or interface in the context: (Method: "+name.value+", "+name.getPos+")")
                }
            case _: ArgumentDecl =>
                // Skip SimpleVariables contained inside arguments declarations
                continue = false;


            case SimpleVariable(id) =>
                ctx.varScope.lookupVariable(id.value) match {
                    case Some(vs) =>
                        id.setSymbol(vs)
                    case None =>
                        val vs = new VariableSymbol(id.value).setPos(id)
                        id.setSymbol(vs);
                        ctx.varScope.registerVariable(vs)
                }

            case StaticClassRef(_, _, id) =>
                GlobalSymbols.lookupClass(id.value) match {
                    case Some(cs) =>
                        id.setSymbol(cs)
                    case None =>
                        ctx.cl match {
                            case Some(cs) =>
                                if (id.value == "self") {
                                    id.setSymbol(cs)
                                } else if (id.value == "parent") {
                                    cs.parent match {
                                        case Some(pcs) =>
                                            id.setSymbol(cs)
                                        case None =>
                                            Reporter.error("Class '"+cs.name+"' has no parent", id);
                                    }
                                }
                            case None =>
                                if (id.value == "self" || id.value == "parent") {
                                    Reporter.error(id.value+" cannot be used outside of a class definition", id);
                                }
                        }
                }

            case _ =>
        }

        (newCtx, continue)
    }

    def getVisibility(flags: List[MemberFlag]): MemberVisibility = flags match {
        case MFPublic :: xs => MVPublic
        case MFProtected :: xs => MVProtected
        case MFPrivate :: xs => MVPrivate
        case _ :: xs => getVisibility(xs)
        case Nil => MVPublic
    }

    def execute = {
        traverse(visitClasses)

        firstClassPass;
        for(c <- classList) {
            secondClassPass(c._2, c._1);
        }

        traverse(visit)

        GlobalSymbols.registerPredefVariables
    }

}
