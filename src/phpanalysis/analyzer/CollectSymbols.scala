package phpanalysis.analyzer;

import phpanalysis.parser.Trees._
import phpanalysis.analyzer.Symbols._
import phpanalysis.analyzer.Types._

import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap

case class Context(varScope: Scope, cl: Option[ClassSymbol], iface: Option[IfaceSymbol]);

case class CollectSymbols(node: Tree) extends ASTTraversal[Context](node, Context(GlobalSymbols, None, None)) {
    var classCycleDetectionSet = new HashSet[ClassDecl]
    var classesToPass = List[ClassDecl]()
    var classList: List[(ClassSymbol, ClassDecl)] = Nil

    /**
     * Visit classes and add them to a waiting list, so they can be processed in right order
     */
    def visitClasses(node: Tree, ctx: Context): (Context, Boolean) = {
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
        case None => {
          firstClassPass0(cd)
          firstClassPass
        }
        case Some(x) => Reporter.error("Class " + x.name + " already declared (previously declared at "+x.getPos+")", cd)
      }
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
      cd.name.setSymbol(cs)

      classList = classList ::: List((cs,cd))
      classCycleDetectionSet -= cd

      classesToPass = classesToPass.remove(_.equals(cd))
    }

    def typeFromExpr(oe: Option[Expression]): Type = oe match {
        case Some(e) => typeFromExpr(e)
        case None => TNull
    }
    def typeFromExpr(e: Expression): Type = e match {
        case PHPTrue() => TTrue
        case PHPFalse() => TFalse
        case PHPInteger(_) => TInt
        case PHPFloat(_) => TFloat
        case PHPString(_) => TString
        case PHPNull() => TNull
        case MCFile() => TString
        case MCLine() => TString
        case MCDir() => TString
        case MCClass() => TString
        case MCFunction()  => TString
        case MCMethod() => TString
        case MCNamespace() => TString
        case Minus(_, _) => TInt
        case a: Array =>
            //TODO
            TAnyArray
        case _=>
            println("Woops, getting type from a non-scalar expression: "+e);
            TAny
    }
    
    def secondClassPass(cd: ClassDecl, cs: ClassSymbol): Unit = {
        for (val m <- cd.methods) {
            val ms = new MethodSymbol(cs, m.name.value, getVisibility(m.flags)).setPos(m)
            cs.registerMethod(ms)
            m.name.setSymbol(ms)
            for (a <- m.args) {
                val vs = new VariableSymbol(a.v.name.value).setPos(a.v)
                var t: Type = a.hint match {
                    case Some(THString) => TString
                    case Some(THInt) => TInt
                    case Some(THBoolean) => TBoolean
                    case Some(THFloat) => TInt // TODO: Differientate numeric types
                    case Some(THArray) => TAnyArray
                    case Some(o: THObject) => TAnyObject // TODO: Make it more precise
                    case None => TAny;
                }
                if (a.default == Some(PHPNull)) {
                    /*
                     * PHP Hack: if you pass null as default value, then null
                     * is also accepted as type
                     */
                     t = TUnion(t, TNull)
                }

                ms.registerArgument(vs, a.byref, t, a.default != None);
            }
        }

        for (val p <- cd.props) {
            val ps = new PropertySymbol(cs, p.v.value, getVisibility(p.flags), typeFromExpr(p.default)).setPos(p)
            cs.registerProperty(ps)
        }

        for (val p <- cd.static_props) {
            val ps = new PropertySymbol(cs, p.v.value, getVisibility(p.flags), typeFromExpr(p.default)).setPos(p)
            cs.registerStaticProperty(ps)
        }

        for (val c <- cd.consts) {
            val ccs = new ClassConstantSymbol(cs, c.v.value, typeFromExpr(c.value)).setPos(c)
            cs.registerConstant(ccs)
        }
    }

    /**
     * Visit the nodes and aggregate information inside the context to provide
     * hints about obvious errors directly from the AST
     */
    def visit(node: Tree, ctx: Context): (Context, Boolean) = {
        var newCtx = ctx;
        var continue = true;

        node match {
            case FunctionDecl(name, args, retref, body) =>
                val fs = new FunctionSymbol(name.value).setPos(name)
                for (val a <- args) {
                    val vs = new VariableSymbol(a.v.name.value).setPos(a.v)
                    var t: Type = a.hint match {
                        case Some(THString) => TString
                        case Some(THInt) => TInt
                        case Some(THBoolean) => TBoolean
                        case Some(THFloat) => TInt // TODO: Differientate numeric types
                        case Some(THArray) => TAnyArray
                        case Some(o: THObject) => TAnyObject // TODO: Make it more precise
                        case None => TAny;
                    }

                    if (a.default == Some(PHPNull)) {
                        /*
                         * PHP Hack: if you pass null as default value, then null
                         * is also accepted as type
                         */
                         t = TUnion(t, TNull)
                    }

                    fs.registerArgument(vs, a.byref, t, a.default != None);
                }
                name.setSymbol(fs)
                GlobalSymbols.registerFunction(fs)
                newCtx = Context(fs, None, None)

            case ClassDecl(name, flags, parent, interfaces, methods, static_props, props, consts) =>
                GlobalSymbols.lookupClass(name.value) match {
                    case Some(cs) =>
                        newCtx = Context(ctx.varScope, Some(cs), None)
                    case None => error("Woops ?!? Came across a phantom class");
                }

            case InterfaceDecl(name, parents, methods, consts) =>
                GlobalSymbols.lookupIface(name.value) match {
                    case Some(iface) =>
                        newCtx = Context(ctx.varScope, None, Some(iface))
                    case None => error("Woops ?!? Came across a phantom interface");
                }

            case MethodDecl(name, flags, args, retref, body) =>
                (ctx.cl, ctx.iface) match {
                    case (Some(cs), _) => cs.lookupMethod(name.value, Some(cs)) match {
                        case LookupResult(Some(ms: MethodSymbol), _, _) => newCtx = Context(ms, Some(cs), None)
                        case _ => error("Woops?! No such method declared yet??")
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
                                } else if (id.value == "static") {
                                    // can't do much with that, it's runtime dependant
                                    // setting symbol of self..
                                    id.setSymbol(cs)
                                }
                            case None =>
                                if (id.value == "self" || id.value == "parent" || id.value == "static") {
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
        for (val c <- classList) {
            secondClassPass(c._2, c._1);
        }

        traverse(visit)
    }

}
