package phantm.types

import phantm.Settings
import phantm.util.{Reporter, Positional}

import phantm.ast.{Trees => AST}
import phantm.cfg.ControlFlowGraph
import phantm.cfg.Trees._
import phantm.symbols._
import phantm.phases.PhasesContext
import phantm.annotations.{AnnotationsStore, SourceAnnotations}
import phantm.dataflow.AnalysisAlgorithm
import phantm.cfg.{LabeledDirectedGraphImp, VertexImp}

case class TypeFlowAnalyzer(cfg: ControlFlowGraph, scope: Scope, ctx: PhasesContext, inlined: Boolean = false) {

    type Vertex = VertexImp[Statement]

    def setupEnvironment: TypeEnvironment = {
        var baseEnv   = new TypeEnvironment;

        // Get data from dumped state if any
        def getSuperGlobal(name: String): Type = {
            if (ctx.dumpedData != Nil) {
                val map = ctx.dumpedData.flatMap(d => d.heap.toTypeMap).toMap
                map.getOrElse(name, TNull)
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

        injectPredef("GLOBALS",  ctx.globals.getOrElse(new TArray(TAny)))

        // for methods, we inject $this as its always defined
        scope match {
            case ms: MethodSymbol =>
                baseEnv = baseEnv.setStore(baseEnv.store.initIfNotExist(ObjectId(-1, 0), Some(ms.cs)))
                injectPredef("this", new TObjectRef(ObjectId(-1, 0)))
            case _ =>
        }

        // in case we have a function or method symbol, we also inject arguments
        scope match {
            case fs: FunctionSymbol =>
                for ((name, sym) <- fs.argList) {
                    baseEnv = baseEnv.inject(Identifier(sym), sym.typ)
                }
            case _ =>
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


        val aa = new AnalysisAlgorithm[TypeEnvironment, Statement](TypeTransferFunction(true, newCtx, false), bottomEnv, baseEnv, cfg)

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
            noticesCount += 1;
            Reporter.notice(msg, pos);
        }

        // Detect unreachables:
        if (ctx.dumpedData.isEmpty && !inlined) {
            // Only do it if no runtime instrumentation
            for (l <- aa.detectUnreachable(TypeTransferFunction(true, newCtx, false))) {
                notice("Unreachable code", l)
            }
        }
        // Collect errors and annotations
        aa.pass(TypeTransferFunction(false, newCtx, !Settings.get.exportAPIPath.isEmpty, inlined, notice))

        if (Settings.get.summaryOnly && !inlined) {
            scope match {
                case ms: MethodSymbol =>
                    val lineCount = ms.line_end-ms.line+1;
                    val isAnnotated = SourceAnnotations.Parser.isAnnotated(ms.comment.getOrElse(""))
                    printf(" %3d | %3d | %.2f | %3s | %-50s | %s \n", noticesCount,
                                                                      lineCount,
                                                                      noticesCount*1.0/lineCount,
                                                                      if (isAnnotated) "yes" else "no",
                                                                      ms.cs.name+"::"+ms.name,
                                                                      ms.file.map(f => f+":"+ms.line).getOrElse("-- no file --"));
                case fs: FunctionSymbol =>
                    val lineCount = fs.line_end-fs.line+1;
                    val isAnnotated = SourceAnnotations.Parser.isAnnotated(fs.comment.getOrElse(""))
                    printf(" %3d | %3d | %.2f | %3s | %-50s | %s \n", noticesCount,
                                                                      lineCount,
                                                                      noticesCount*1.0/lineCount,
                                                                      if (isAnnotated) "yes" else "no",
                                                                      fs.name,
                                                                      fs.file.map(f => f+":"+fs.line).getOrElse("-- no file --"));
                case _ =>
            }
        }

        aa.getResult
    }
}
