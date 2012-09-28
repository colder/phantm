package phantm.phases

import phantm._
import helpers.NamespaceContext
import phantm.util.{Reporter, Positional}
import phantm.ast.Trees._
import ast.ASTTraversal
import phantm.types._
import phantm.symbols._
import phantm.annotations.SourceAnnotations.{Parser => CommentParser}
import scala.None
import scala.collection.mutable.HashSet

object SymbolsCollectionPhase extends Phase {

    def name = "Symbols collection"
    def description = "Collecting symbols"

    def run(ctx: PhasesContext): PhasesContext = {
        CollectSymbols(ctx.oast.get) execute;
        ctx
    }

}

case class IsA[T](clazz : Class[_ <:  T]) {
  def unapply(obj : T):Option[T] = {
    if (clazz.isInstance(obj))  Some(obj) else  None
  }
}

case class SymContext(varScope: Scope, cl: Option[ClassSymbol], iface: Option[IfaceSymbol]);

case class CollectSymbols(node: Tree) extends ASTTraversal[SymContext](node, SymContext(GlobalSymbols, None, None)) {
    var classCycleDetectionSet = new HashSet[ClassDecl]
    var ifaceCycleDetectionSet = new HashSet[InterfaceDecl]
    var classesToPass = List[AnyRef]()
    var interfacesToPass = List[AnyRef]()
    var classList: List[(ClassSymbol, ClassDecl)] = Nil
    var ifaceList: List[(IfaceSymbol, InterfaceDecl)] = Nil
     var nsContext : NamespaceContext = new NamespaceContext
    /**
     * Visit classes and add them to a waiting list, so they can be processed in right order
     */
    def visitClasses(node: Tree, ctx: SymContext): (SymContext, Boolean) = {
        node match {
          case use : UseStatement  =>
               nsContext.addUseStatement(use)
          case ns : NSDeclaration  =>
                nsContext.setNamespace(ns)
               classesToPass = classesToPass ::: List(nsContext.clone)
               interfacesToPass = interfacesToPass ::: List(nsContext.clone)
            case cl : ClassDecl =>
                classesToPass = classesToPass ::: List(cl)
            case id : InterfaceDecl =>
                interfacesToPass = interfacesToPass ::: List(id)
            case _ =>
        }
        (ctx, true)
    }

    val IsNamespaceContext = new IsA(classOf[NamespaceContext])
    val IsClassDecl = new IsA(classOf[ClassDecl])
    val IsInterfaceDecl = new IsA(classOf[InterfaceDecl])

    def firstIfacePass(interfaces : List[ AnyRef], ns: NamespaceContext) : Unit = {
      interfaces match {

        case Nil =>
        case IsNamespaceContext(ns1) :: theRest =>
          firstIfacePass(theRest,ns1.asInstanceOf[NamespaceContext])
        case IsInterfaceDecl(id) :: theRest =>
          GlobalSymbols.lookupIface(id.qName(ns)) match {
            case None =>
              firstIfacePass0(ns, id.asInstanceOf[InterfaceDecl])
            case Some(x) =>
              Reporter.notice("Interface " + x.qName + " already declared (previously declared at "+x.getPos+")", id)
          }
          firstIfacePass(theRest, ns)
      }
     }

    def firstIfacePass0(ns: NamespaceContext, id: InterfaceDecl): Unit = {
      if (ifaceCycleDetectionSet.contains(id)) {
        Reporter.error("Interface " + ifaceCycleDetectionSet.map(x => x.qName(ns)).mkString(" -> ") + " form an inheritance cycle", id)
        return;
      }
      ifaceCycleDetectionSet += id

      var parentIfaces: List[IfaceSymbol] = Nil;

      for (pi <- id.interfaces) {
          pi match {
            case scr @ StaticClassRef(_, _, _) =>
              GlobalSymbols.lookupIface(scr.qName(ns)) match {
                  case None => {
                    var foundParent = false
                    for (i <-interfacesToPass.asInstanceOf[List[InterfaceDecl]] if i.qName(ns).equals(scr.qName(ns)) && !foundParent) {
                      firstIfacePass0(ns, i.asInstanceOf)
                      foundParent = true
                    }

                    GlobalSymbols.lookupIface(scr.qName(ns)) match {
                      case None =>
                        Reporter.error("Interface " + id.qName(ns) + " extends non-existent interface " + scr.qName(ns), id)
                      case Some(pis) =>
                        parentIfaces = pis :: parentIfaces
                    }
                  }
                  case Some(pis) => parentIfaces = pis :: parentIfaces
              }
            case _ =>

          }
      }
      val is = new IfaceSymbol(ns.getCurrent.names, id.name.value, parentIfaces).setPos(id).setUserland;
      GlobalSymbols.registerIface(is)

      ifaceList = ifaceList ::: List((is,id))
      ifaceCycleDetectionSet -= id
    }

    def secondIfacePass(id: InterfaceDecl, is: IfaceSymbol): Unit = {
        for (m <- id.methods) {
            val ims = new IfaceMethodSymbol(is, m.name.value, getVisibility(m.flags)).setPos(m).setUserland
            is.registerMethod(ims)

            for (a <- m.args) {
                var t = TypeHelpers.typeHintToType(a.hint)

                if (a.default == Some(PHPNull)) {
                    /*
                     * PHP Hack: if you pass null as default value, then null
                     * is also accepted as type
                     */
                     t = TUnion(t, TNull)
                }

                val as = new ArgumentSymbol(a.v.name.value, a.byref, a.default != None).setPos(a.v).setUserland
                as.typ = t
                ims.registerArgument(as);
            }
        }
    }

    def firstClassPass(classes : List[AnyRef], ns: NamespaceContext) : Unit = {
     classes match {
      case Nil =>
      case IsNamespaceContext(ns1) :: theRest =>
         firstClassPass(theRest, ns1.asInstanceOf[NamespaceContext])
      case IsClassDecl(cd) :: theRest =>
          GlobalSymbols.lookupClass(cd.qName(ns)) match {
            case None =>
              firstClassPass0(ns, cd.asInstanceOf[ClassDecl])
            case Some(x) =>
              Reporter.notice("Class " + x.qName + " already declared (previously declared at "+x.getPos+")", cd)
          }
          firstClassPass(theRest, ns)

     }

    }

    def firstClassPass0(ns: NamespaceContext, cd: ClassDecl): Unit = {
      if (classCycleDetectionSet.contains(cd)) {
        Reporter.error("Classes " + classCycleDetectionSet.map(x => x.qName(ns)).mkString(" -> ") + " form an inheritance cycle", cd)
        return;
      }

      classCycleDetectionSet += cd

      val p: Option[ClassSymbol] = cd.parent match {
        case Some(x) => GlobalSymbols.lookupClass(x.qName(ns)) match {
          case None => {
            var foundParent = false
            for (c <- classesToPass.asInstanceOf[List[ClassDecl]] if c.qName(ns).equals(x.qName(ns)) && !foundParent) {
              firstClassPass0(ns,c)
              foundParent = true
            }

            GlobalSymbols.lookupClass(x.qName(ns)) match {
              case None => Reporter.error("Class " + cd.qName(ns) + " extends non-existent class " + x.qName(ns), x); None
              case x => x
            }
          }
          case Some(pcs) => Some(pcs)
        }
        case None => None
      }

      var ifaces = List[IfaceSymbol]()
      for (i <- cd.interfaces) i match {
        case StaticClassRef(_, _, id) =>
            GlobalSymbols.lookupIface(id.value) match {
                case Some(is) =>
                    ifaces = is :: ifaces
                case None =>
                    Reporter.error("Class "+cd.qName(ns) +" implements non-existent interface "+id.value);
            }
        case _ =>
      }

      val cs = new ClassSymbol(ns.getCurrent.names,cd.name.value, p, ifaces.reverse).setPos(cd).setUserland;
      GlobalSymbols.registerClass(cs)

      classList = classList ::: List((cs,cd))
      classCycleDetectionSet -= cd
    }


    def secondClassPass(cd: ClassDecl, cs: ClassSymbol): Unit = {
        for (m <- cd.methods) {
            val ms = new MethodSymbol(cs, m.name.value, getVisibility(m.flags)).setPos(m).setUserland
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

                val as = new ArgumentSymbol(a.v.name.value, a.byref, a.default != None).setPos(a.v).setUserland
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

            val ps = new PropertySymbol(cs, p.v.value, getVisibility(p.flags)).setPos(p).setUserland

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

            val ps = new PropertySymbol(cs, p.v.value, getVisibility(p.flags)).setPos(p).setUserland

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
                    new ClassConstantSymbol(cs, c.v.value, Some(sc)).setPos(c).setUserland
                case _ =>
                    new ClassConstantSymbol(cs, c.v.value, None).setPos(c).setUserland
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
            case fd @ FunctionDecl( name, args, retref, body) =>

                // If the function is declared more than once, we discard other definitions
                val lc = fd.qName(nsContext)

                val fs = if (GlobalSymbols.functions contains lc) {
                    // Prevent this importation
                    GlobalSymbols.functions(lc)
                } else {
                    val fs = new FunctionSymbol(nsContext.getCurrent.names,name.value).setPos(fd).setUserland
                    for(a <- args) {
                        var t = TypeHelpers.typeHintToType(a.hint)

                        if (a.default == Some(PHPNull)) {
                            /*
                             * PHP Hack: if you pass null as default value, then null
                             * is also accepted as type
                             */
                             t = t union TNull
                        }

                        val as = new ArgumentSymbol(a.v.name.value, a.byref, a.default != None).setPos(a.v).setUserland
                        as.typ = t

                        fs.registerArgument(as);
                    }
                    GlobalSymbols.registerFunction(fs)
                    fs
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
                    if (t.args.size <= i || args.size <= i) {
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
                name.setSymbol(fs)
                newCtx = SymContext(fs, None, None)

            case cd @ ClassDecl( name, flags, parent, interfaces, methods, static_props, props, consts) =>
                GlobalSymbols.lookupClass(cd.qName(nsContext)) match {
                    case Some(cs) =>
                        name.setSymbol(cs);
                        newCtx = SymContext(ctx.varScope, Some(cs), None)
                    case None => sys.error("Woops ?!? Came across a phantom class: " + cd.qName(nsContext));
                }

            case id @ InterfaceDecl( name, parents, methods, consts) =>
                GlobalSymbols.lookupIface(id.qName(nsContext)) match {
                    case Some(iface) =>
                        newCtx = SymContext(ctx.varScope, None, Some(iface))
                    case None => sys.error("Woops ?!? Came across a phantom interface: " + id.qName(nsContext));
                }

            case MethodDecl(name, flags, args, retref, body) =>
                (ctx.cl, ctx.iface) match {
                    case (Some(cs), _) => cs.lookupMethod(name.value, Some(cs)) match {
                        case LookupResult(Some(ms: MethodSymbol), _, _) =>
                            name.setSymbol(ms)
                            newCtx = SymContext(ms, Some(cs), None)
                        case _ => sys.error("Woops?! No such method declared yet: "+cs.name+"::"+name.value+" ??")
                    }
                    case (None, Some(iface)) =>
                        name.setSymbol(iface)
                        // no body
                    case (None, None) =>
                        sys.error("Woops?!? Got into a method without any class or interface in the context: (Method: "+name.value+", "+name.getPos+")")
                }
            case _: ArgumentDecl =>
                // Skip SimpleVariables contained inside arguments declarations
                continue = false;


            case SimpleVariable(id) =>
                ctx.varScope.lookupVariable(id.value) match {
                    case Some(vs) =>
                        id.setSymbol(vs)
                    case None =>
                        val vs = new VariableSymbol(id.value).setPos(id).setUserland
                        id.setSymbol(vs);
                        ctx.varScope.registerVariable(vs)
                }

            case scr @ StaticClassRef(_, _, id) =>
                GlobalSymbols.lookupClass(scr.qName(nsContext)) match {
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
                                            id.setSymbol(pcs)
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
            case use : UseStatement =>
              nsContext.addUseStatement(use)
            case ns : NSDeclaration =>
                nsContext.setNamespace(ns)
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

        firstIfacePass(interfacesToPass, new NamespaceContext);
        for(i <- ifaceList) {
            secondIfacePass(i._2, i._1);
        }

        firstClassPass(classesToPass, new NamespaceContext);
        for(c <- classList) {
            secondClassPass(c._2, c._1);
        }
        nsContext = new NamespaceContext
        traverse(visit)

        GlobalSymbols.registerPredefVariables
    }

}
