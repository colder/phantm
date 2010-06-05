package phantm.phases

import phantm.Settings
import phantm.util.{API, Reporter}
import phantm.symbols._
import phantm.ast.Trees._
import phantm.ast.ASTTraversal
import java.io.{PrintStream,FileOutputStream}

object CallGraphPhase extends Phase {
    def name = "Generating callgraph"
    def description = "Generating call graph"

    def run(ctx: PhasesContext): PhasesContext = {

        if (!Settings.get.exportCGPath.isEmpty) {
            val cgGenerator = new CallGraphGeneration(ctx.oast.get)

            cgGenerator.execute

            val outputStream = new FileOutputStream(Settings.get.exportCGPath.get);
            val printStream  = new PrintStream(outputStream);

            cgGenerator.CallGraph.emitDot(printStream)
        }

        ctx
    }
}

case class CGContext(scope: Option[FunctionSymbol]);

case class CallGraphGeneration(node: Tree,
                              context: CGContext) extends ASTTraversal[CGContext](node, context) {

    object CallGraph {
        type FuncId = Option[FunctionSymbol]
        var nodes = Set[FuncId]()
        var edges = Map[(FuncId, FuncId), Int]().withDefaultValue(0)

        def addNode(f: FuncId) = {
            if (f.isEmpty || f.get.userland) {
                nodes += f
            }
        }

        def addEdge(from: FuncId, to: FuncId) {
            if ((from.isEmpty || from.get.userland) && (to.isEmpty || to.get.userland)) {
                addNode(from)
                addNode(to)
                edges += (from -> to) -> (edges(from -> to) + 1);
            }
        }

        def emitDot(ps: PrintStream) = {
            // we have at least the main scope
            addNode(None);

            var i = 1
            var namesToIds = Map[String, String]()

            ps.print("digraph G {\n")
            def getName(fid: FuncId): String = {
                val l = getLabel(fid)

                if (namesToIds contains l) {
                    namesToIds(l)
                } else {
                    val fresh = "func"+i
                    namesToIds += (l -> fresh)
                    i += 1
                    fresh
                }
            }

            def getLabel(fid: FuncId): String = fid match {
                case Some(ms: MethodSymbol) => ms.cs.name+"::"+ms.name
                case Some(fs: FunctionSymbol) => fs.name
                case _ => "<main>"
            }




            for (node <- nodes) {
                ps.print("  "+getName(node)+" [label=\""+getLabel(node)+"\"]\n")
            }

            for (((from, to), n) <- edges) {
                ps.print("  "+getName(from)+" -> "+getName(to)+" [label=\""+n+"\"]\n")
            }

            ps.print("}\n")
        }
    }

    def this(node: Tree) = this(node, CGContext(None))

    /**
     * Visit the nodes and aggregate information inside the context to provide
     * hints about obvious errors directly from the AST
     */
    def visit(node: Tree, ctx: CGContext): (CGContext, Boolean) = {
        def scopeFromSym(s: Symbol): Option[FunctionSymbol] = s match {
            case fs: FunctionSymbol =>
                Some(fs)
            case _ =>
                None
        }

        var newCtx = ctx

        node match {
            case fd : FunctionDecl =>
                val fid = scopeFromSym(fd.name.getSymbol)
                CallGraph.addNode(fid)
                newCtx = CGContext(fid);
            case md: MethodDecl =>
                val fid = scopeFromSym(md.name.getSymbol)
                CallGraph.addNode(fid)
                newCtx = CGContext(fid);

            case FunctionCall(StaticFunctionRef(_, _, name), args) =>
                GlobalSymbols.lookupFunction(name.value) match {
                    case Some(fs) =>
                        CallGraph.addEdge(ctx.scope, Some(fs))
                    case _ =>
                }

            case _ =>
        }

        (newCtx, true)
    }

    def execute = traverse(visit)
}
