package phantm.types

import phantm.Settings
import phantm.util._

import phantm.ast.{Trees => AST}
import phantm.cfg.ControlFlowGraph
import phantm.cfg.Trees._
import phantm.symbols._
import phantm.phases.PhasesContext
import phantm.annotations.{AnnotationsStore, SourceAnnotations}
import phantm.dataflow.AnalysisAlgorithm
import phantm.cfg.{LabeledDirectedGraphImp, VertexImp}

case class TypeFlowAnalyzer(cfg: ControlFlowGraph,
                            scope: Scope,
                            ctx: PhasesContext,
                            inlined: Boolean = false,
                            collectGlobals: Boolean = false,
                            baseEnvInit: TypeEnvironment = new TypeEnvironment,
                            thisObj: Option[TObjectRef] = None) {

    type Vertex = VertexImp[Statement]

    def setupEnvironment: TypeEnvironment = {
        var baseEnv   = baseEnvInit;

        // Get data from dumped state if any
        def getSuperGlobal(name: String): Type = {
            if (ctx.dumpedData != Nil) {
                val map = ctx.dumpedData.flatMap(d => d.heap.toTypeMap).toMap
                map.getOrElse(name, TNull)
            } else if (Settings.get.anyInput) {
                new TArray(TBottom)
            } else {
                new TArray(TTop)
            }
        }


        // We now inject predefined variables
        def injectPredef(name: String, typ: Type): Unit = {
            scope.lookupVariable(name) match {
                case Some(vs) =>
                    baseEnv = baseEnv.inject(Identifier(vs), typ)
                case None =>
                    // ignore this var
                    println("Woops, no such symbol found: "+name)
            }
        }

        def injectSuperGlobal(name: String): Unit =
            injectPredef(name, getSuperGlobal(name))

        //scope.registerPredefVariables
        injectSuperGlobal("_GET")
        injectSuperGlobal("_POST")
        injectSuperGlobal("_REQUEST")
        injectSuperGlobal("_COOKIE")
        injectSuperGlobal("_SERVER")
        injectSuperGlobal("_FILES")
        injectSuperGlobal("_ENV")
        injectSuperGlobal("_SESSION")


        //

        // for methods, we inject $this as its always defined
        scope match {
            case ms: MethodSymbol =>
                if (thisObj.isEmpty) {
                    // $this is a singleton object
                    baseEnv = baseEnv.setStore(baseEnv.store.initIfNotExist(ObjectId(-1, ObjectIdUse), Some(ms.cs)))
                    injectPredef("this", new TObjectRef(ObjectId(-1, ObjectIdUse)))
                } else {
                    injectPredef("this", thisObj.get)
                }
            case _ =>
        }

        // in case we have a function or method symbol, we also inject arguments
        scope match {
            case fs: FunctionSymbol =>
                for ((name, sym) <- fs.argList) {
                    baseEnv = baseEnv.inject(Identifier(sym), sym.typ)
                }

                // All function symbols invoked from main scope that lead to this function
                val mfs = ctx.results.reachableFromMain(fs)

                val globals = mfs.flatMap(mf => ctx.results.globalCalls(mf))

                var globalsType: Type = new TArray(TAny)

                if (globals.size > 1) {
                    val e = globals.map(_._2).reduceLeft((e1, e2) => e1.union(e2))
                    globalsType = e.getGlobalsType
                    baseEnv = baseEnv unionStoreFrom e
                } else if (globals.size == 1) {
                    val e = globals.head._2
                    globalsType = e.getGlobalsType
                    baseEnv = baseEnv unionStoreFrom e
                } else {
                    if (!ctx.results.endGlobals.isEmpty) {
                        val e = ctx.results.endGlobals.get
                        baseEnv = baseEnv unionStoreFrom e
                        globalsType = e.getGlobalsType
                    }
                }

                injectPredef("GLOBALS",  globalsType)
            case GlobalSymbols =>
                injectPredef("GLOBALS",  new TArray(TAny))
        }

        // we inject vars for static class properties
        for(cs <- GlobalSymbols.getClasses) {
            for(ps <- cs.getStaticProperties) {
                baseEnv = baseEnv.inject(ClassProperty(ps), ps.typ)
            }
        }

        baseEnv
    }

    def analyze: Map[Vertex, TypeEnvironment] = {
        val bottomEnv = BaseTypeEnvironment;
        val baseEnv   = setupEnvironment;
        var newCtx = ctx

        scope match {
            case fs: FunctionSymbol =>
                newCtx = newCtx.copy(symbol = Some(fs))
            case _ =>
        }

        val ttf = TypeTransferFunction(true, newCtx, false, collectGlobals)
        val aa = new AnalysisAlgorithm[TypeEnvironment, Statement](ttf, bottomEnv, baseEnv, cfg)

        aa.computeFixpoint(newCtx)

        if (Settings.get.displayFixPoint && !inlined) {
            println("     - Fixpoint:");
            for ((v,e) <- aa.getResult.filter(v => !v._1.isInstanceOf[ClassProperty]).toList.sortWith{(x,y) => x._1.name < y._1.name}) {
                println("      * ["+v+"] => "+e);
            }
        }

        // Collect error summaries per function/method
        var noticesCount = 0;

        def notice(msg: String, pos: Positional) = {
            if (Reporter.notice(msg, pos)) {
                noticesCount += 1;
            }
        }

        // Detect unreachables:
        if (!inlined) {
            // Only do it if not inlined
            for (l <- aa.detectUnreachable(TypeTransferFunction(true, newCtx, false))) {
                if (ctx.dumpedData.isEmpty) {
                    notice("Unreachable code", l)
                } else {
                    notice("Code unreachable given the runtime data", l)
                }
            }
        }
        // Collect errors and annotations
        aa.pass(TypeTransferFunction(false, newCtx, !Settings.get.exportAPIPath.isEmpty, collectGlobals, inlined, notice))

        if (Settings.get.summaryOnly) {
            val gr = ctx.results
            scope match {
                case fs: FunctionSymbol =>
                    gr.summary = gr.summary + (fs -> (gr.summary.getOrElse(fs, 0) + noticesCount))
                case _ =>
            }
        }

        aa.getResult
    }
}
