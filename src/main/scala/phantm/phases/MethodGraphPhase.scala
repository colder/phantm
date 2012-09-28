package phantm.phases

import phantm._
import phantm.util.{API, Reporter, Positional}
import phantm.symbols._
import phantm.ast.Trees._
import phantm.ast.ASTTraversal
import phantm.cfg.LabeledDirectedGraphImp
import phantm.dataflow.StronglyConnectedComponents
import java.io.{PrintStream,FileOutputStream}

object MethodGraphPhase  extends Phase {
    def name = "Generating methodgraph"
    def description = "Generating method inheritence graph"

    def run(initCtx: PhasesContext): PhasesContext = {
        var ctx = initCtx

        val mgGenerator = new MethodGraphGeneration(ctx)
        mgGenerator.execute

        val mg = mgGenerator.MethodGraph;

        if (!Settings.get.exportMGPath.isEmpty) {
            mg.writeDottyToFile(Settings.get.exportMGPath.get, "Methods Graph")
        }

        // Flag symbols to inline
        if (Settings.get.inlineMode == InlineLeaves || Settings.get.inlineMode == InlineFull) {
            mg.V.filter(v => mg.outEdges(v).isEmpty).foreach(v => mg.vToSym(v).shouldInline = true)
        }

        ctx
    }
}

class MethodGraphGeneration(ctx: PhasesContext) {

    object MethodGraph extends LabeledDirectedGraphImp[String] {
        type AVertex = MethodSymbol

        val entry = newVertex
        entry.name ="\"[main]\""

        var symToV = Map[MethodSymbol, Vertex]()
        var vToSym = Map[Vertex, MethodSymbol]()

        def addNode(sym: AVertex): Vertex = {
            symToV.get(sym) match {
                case Some(v) => v
                case None =>
                    val v = newVertex
                    v.name = "\""+sym.cs.name+"::"+sym.name+"\""
                    symToV += (sym -> v)
                    vToSym += (v -> sym)
                    this += v
                    v
            }
        }

        def addEdge(from: AVertex, to: AVertex) = {
            val vFrom = addNode(from)
            val vTo   = addNode(to)

            this += (vFrom, "...", vTo)
        }
    }


    def execute = {
        // Parent Classes first
        for ((cname, cs) <- ctx.globalSymbols.classes if cs.userland) {
            for ((mname, ms) <- cs.methods) {
                // look for parent method
                lookupParentMethod(cs.parent, ms) match {
                    case Some(pms) =>
                        MethodGraph.addEdge(pms, ms)
                    case None =>
                        MethodGraph.addNode(ms)
                }
            }
        }

    }

    def lookupParentMethod(cs: Option[ClassSymbol], ms: MethodSymbol): Option[MethodSymbol] = cs match {
        case Some(cs) =>
            cs.methods.get(ms.name) match {
                case Some(pms) =>
                    Some(pms)
                case None =>
                    lookupParentMethod(cs.parent, ms)
            }

        case None =>
            None
    }
}
