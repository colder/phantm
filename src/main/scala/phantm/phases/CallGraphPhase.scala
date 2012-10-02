package phantm.phases

import phantm._
import phantm.util.{API, Reporter, Positional}
import phantm.symbols._
import phantm.ast.Trees._
import phantm.ast.ASTTraversal
import phantm.cfg.LabeledDirectedGraphImp
import phantm.dataflow.StronglyConnectedComponents
import java.io.{PrintStream,FileOutputStream}

object CallGraphPhase extends Phase {
    def name = "Generating callgraph"
    def description = "Generating call graph"

    def run(initCtx: PhasesContext): PhasesContext = {
        var ctx = initCtx

        val cgGenerator = new CallGraphGeneration(ctx.oast.get, ctx)
        cgGenerator.execute

        val cg = cgGenerator.CallGraph;

        ctx.results.reachableFromMain = cg.computeReachableFromMain

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
                    val v = ssc.vs.head

                    // Is it self-recursive?
                    if (cg.outEdges(v).exists(e => e.v2 == v)) {
                        flag(cg.vToOsym(v), false)
                    } else {
                        flag(cg.vToOsym(v), true)
                    }
                }
            }
        }

        ctx
    }
}

case class CGContext(scope: Option[FunctionSymbol]);

case class CallGraphGeneration(node: Tree,
                              context: CGContext,
                              pctx: PhasesContext) extends ASTTraversal[CGContext](node, context) {

    object CallGraph extends LabeledDirectedGraphImp[Int] {
        type AVertex = Option[FunctionSymbol]

        val entry = newVertex
        entry.name ="\"[main]\""

        var mainCallPositions = Map[FunctionSymbol, Set[Positional]]().withDefaultValue(Set())

        var osymToV = Map[Option[FunctionSymbol], Vertex](None -> entry)
        var vToOsym = Map[Vertex, Option[FunctionSymbol]](entry -> None)

        def addNode(osym: AVertex): Vertex = {
            osymToV.get(osym) match {
                case Some(v) => v
                case None =>
                    val v = newVertex
                    v.name = osym.map(_.name).getOrElse("??")
                    osymToV += (osym -> v)
                    vToOsym += (v -> osym)
                    v
            }
        }

        def addCallLocation(sym: FunctionSymbol, pos: Positional) = {
            mainCallPositions += (sym -> (mainCallPositions(sym) + pos))
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
                    sys.error("More than one edge between call vertices")
            }
        }

        def computeReachableFromMain = {
            var res  = Map[FunctionSymbol, Set[FunctionSymbol]]().withDefaultValue(Set())
            var mainFuncs = outEdges(entry).map(_.v2);

            for (mf <- mainFuncs) {
                var visited = Set[Vertex]() + mf;
                val msym = vToOsym(mf).get
                res = res + (msym -> Set(msym))

                var toVisit = Set[Vertex]() ++ outEdges(mf).map(_.v2)

                while (!toVisit.isEmpty) {
                    val v = toVisit.head
                    toVisit -= v

                    if (!(visited contains v)) {
                        val sym = vToOsym(v).get
                        res += (sym -> (res(sym) + msym))
                        visited += v
                        toVisit ++ outEdges(v).map(_.v2)
                    }
                }

            }

            res
        }
    }

    def this(node: Tree, pctx: PhasesContext) = this(node, CGContext(None), pctx)

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

            case fcall @ FunctionCall(StaticFunctionRef(id), args) =>
                pctx.globalSymbols.lookupFunction(id.value) match {
                    case Some(fs) if (fs.userland) =>
                        if (ctx.scope == None) {
                            CallGraph.addCallLocation(fs, fcall);
                        }
                        CallGraph.addEdge(ctx.scope, Some(fs))
                    case _ =>
                }
            case fcall @ StaticMethodCall(StaticClassRef(id), StaticMethodRef(mid), args) =>
                pctx.globalSymbols.lookupClass(id.value) match {
                    case Some(cs) =>
                        val cscope = ctx.scope match {
                            case ms: MethodSymbol =>
                                Some(ms.cs)
                            case _ =>
                                None
                        }
                        cs.lookupMethod(mid.value, cscope).ms match {
                            case Some(ms) =>
                                if (ctx.scope == None) {
                                    CallGraph.addCallLocation(ms, fcall);
                                }
                                CallGraph.addEdge(ctx.scope, Some(ms))

                            case None =>
                                //ignore, the error will be reported by other parts
                        }

                    case None =>
                        //ignore, the error will be reported by other parts
                }
            case _ =>
        }

        (newCtx, true)
    }

    def execute = traverse(visit)
}
