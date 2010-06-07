package phantm.phases

import phantm._
import phantm.util.{API, Reporter}
import phantm.symbols._
import phantm.ast.Trees._
import phantm.ast.ASTTraversal
import phantm.cfg.LabeledDirectedGraphImp
import phantm.dataflow.StronglyConnectedComponents
import java.io.{PrintStream,FileOutputStream}

object CallGraphPhase extends Phase {
    def name = "Generating callgraph"
    def description = "Generating call graph"

    def run(ctx: PhasesContext): PhasesContext = {

        val cgGenerator = new CallGraphGeneration(ctx.oast.get)
        cgGenerator.execute

        val cg = cgGenerator.CallGraph;

        if (!Settings.get.exportCGPath.isEmpty) {
            cg.writeDottyToFile(Settings.get.exportCGPath.get, "Call Graph")
        }

        def flag(osym: Option[FunctionSymbol], inline: Boolean) = {
            if (osym != None) {
                osym.get.shouldInline = inline
            }
        }

        // Flag symbols to inline
        if (Settings.get.inlineMode == InlineLeaves) {
            cg.V.filter(v => cg.outEdges(v).isEmpty && !cg.inEdges(v).isEmpty).foreach(v => flag(cg.vToOsym(v), true))

        } else if (Settings.get.inlineMode == InlineFull) {
            // Detect cycles
            val tarjan = new StronglyConnectedComponents(cg)
            val sscs = tarjan.getComponents

            for (ssc <- sscs) {
                if (ssc.vs.size > 1) {
                    for (v <- ssc.vs) flag(cg.vToOsym(v), false)
                } else {
                    for (v <- ssc.vs) flag(cg.vToOsym(v), true)
                }
            }
        }

        ctx
    }
}

case class CGContext(scope: Option[FunctionSymbol]);

case class CallGraphGeneration(node: Tree,
                              context: CGContext) extends ASTTraversal[CGContext](node, context) {

    object CallGraph extends LabeledDirectedGraphImp[Int] {
        type AVertex = Option[FunctionSymbol]

        val entry = newVertex

        var osymToV = Map[Option[FunctionSymbol], Vertex](None -> entry)
        var vToOsym = Map[Vertex, Option[FunctionSymbol]](entry -> None)

        def addNode(osym: AVertex): Vertex = {
            osymToV.get(osym) match {
                case Some(v) => v
                case None =>
                    val v = newVertex
                    v.name = osym.map(_.name).getOrElse("<main>")
                    osymToV += (osym -> v)
                    vToOsym += (v -> osym)
                    v
            }
        }

        def addEdge(from: AVertex, to: AVertex) = {
            val vFrom = addNode(from)
            val vTo   = addNode(to)

            betweenEdges(vFrom, vTo).toList  match {
                case Nil =>
                    this += (vFrom, 1, vTo)
                case e :: Nil =>
                    this -= (vFrom, e.lab, vTo)
                    this += (vFrom, e.lab+1, vTo)
                case _ =>
                    error("More than one edge between call vertices")
            }
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
                    case Some(fs) if (fs.userland) =>
                        CallGraph.addEdge(ctx.scope, Some(fs))
                    case _ =>
                }

            case _ =>
        }

        (newCtx, true)
    }

    def execute = traverse(visit)
}
