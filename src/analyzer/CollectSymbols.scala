package phpanalysis.analyzer;

import phpanalysis.parser.Trees._
import phpanalysis.analyzer.Symbols._
import phpanalysis.analyzer.Types._

import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap

case class Context(varScope: Scope, cl: Option[ClassSymbol]);

case class CollectSymbols(node: Tree) extends ASTTraversal[Context](node, Context(GlobalSymbols, None)) {
    var cycleDetectionSet = new HashSet[ClassDecl]
    var classesToPass = List[ClassDecl]()
    var list: List[(ClassSymbol, ClassDecl)] = Nil

    /**
     * Visit classes and add them to a waiting list, so they can be processed in right order
     */
    def visitClasses(node: Tree, ctx: Context): (Context, Boolean) = {
        node match {
            case cl @ ClassDecl(name, flags, parent, interfaces, methods, static_props, props, consts) =>
                classesToPass = classesToPass ::: List(cl)
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
      if (cycleDetectionSet.contains(cd)) { 
        Reporter.error("Classes " + cycleDetectionSet.map(x => x.name.value).mkString(" -> ") + " form an inheritance cycle", cd)
        return;
      }

      cycleDetectionSet += cd
      
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

      list = list ::: List((cs,cd))
      cycleDetectionSet -= cd

      classesToPass = classesToPass.remove(_.equals(cd))
    }
    
    def secondClassPass(cd: ClassDecl, cs: ClassSymbol): Unit = {
        for (val m <- cd.methods) {
            val ms = new MethodSymbol(cs, m.name.value, getVisibility(m.flags)).setPos(m)
            cs.registerMethod(ms)
            for (a <- m.args) {
                // TODO: type hints
                val vs = new VariableSymbol(a.v.name.value).setPos(a.v)
                ms.registerArgument(vs, a.byref);
            }
        }

        for (val p <- cd.props) {
            val ps = new PropertySymbol(cs, p.v.value, getVisibility(p.flags)).setPos(p)
            cs.registerProperty(ps)
        }

        for (val p <- cd.static_props) {
            val ps = new PropertySymbol(cs, p.v.value, getVisibility(p.flags)).setPos(p)
            cs.registerStaticProperty(ps)
        }

        for (val c <- cd.consts) {
            val ccs = new ClassConstantSymbol(cs, c.v.value).setPos(c)
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
                    // TODO: type hints
                    val vs = new VariableSymbol(a.v.name.value).setPos(a.v)
                    fs.registerArgument(vs, a.byref);
                }
                GlobalSymbols.registerFunction(fs)
                newCtx = Context(fs, None)

            case ClassDecl(name, flags, parent, interfaces, methods, static_props, props, consts) =>
                GlobalSymbols.lookupClass(name.value) match {
                    case Some(cs) =>
                        newCtx = Context(ctx.varScope, Some(cs))
                    case None => error("Woops ?!? Came across a phantom class");
                }

            case MethodDecl(name, flags, args, retref, body) =>
                ctx.cl match {
                    case Some(cs) => cs.lookupMethod(name.value, Some(cs)) match {
                        case LookupResult(Some(ms: MethodSymbol), _, _) => newCtx = Context(ms, None)
                        case _ => error("Woops?! No such method declared yet??")
                    }
                    case None =>
                        error("Woops?!? Got into a method without any class in the context!?!")
                }
            case _: ArgumentDecl =>
                // Skip SimpleVariables contained inside arguments declarations
                continue = false;


            case SimpleVariable(id) =>
                val vs = new VariableSymbol(id.value).setPos(id)
                id.setSymbol(vs);
                ctx.varScope.registerVariable(vs)
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
        for (val c <- list) {
            secondClassPass(c._2, c._1);
        }

        traverse(visit)
    }

}
