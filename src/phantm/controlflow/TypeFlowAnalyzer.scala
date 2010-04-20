package phantm.controlflow

import phantm.Main
import phantm.util.Reporter

import phantm.AST.{Trees => AST}
import phantm.CFG.ControlFlowGraph
import phantm.CFG.Trees._
import phantm.symbols._
import phantm.types._

case class TypeFlowAnalyzer(cfg: ControlFlowGraph, scope: Scope) {

    def setupEnvironment: TypeEnvironment = {
        var baseEnv   = new TypeEnvironment;

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

        //scope.registerPredefVariables
        injectPredef("_GET",     new TArray(TTop))
        injectPredef("_POST",    new TArray(TTop))
        injectPredef("GLOBALS",  new TArray(TTop))
        injectPredef("_REQUEST", new TArray(TTop))
        injectPredef("_COOKIE",  new TArray(TTop))
        injectPredef("_SERVER",  new TArray(TTop))
        injectPredef("_FILES",   new TArray(TTop))
        injectPredef("_ENV",     new TArray(TTop))
        injectPredef("_SESSION", new TArray(TTop))

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

        // Lets try the unserializer
        if (Main.dumpedData != Nil) {
            for (unser <- Main.dumpedData) {
                baseEnv = unser.importToEnv(baseEnv)
            }
        }

        baseEnv
    }

    def analyze: Unit = {
        val bottomEnv = BaseTypeEnvironment;
        val baseEnv   = setupEnvironment;

        val aa = new AnalysisAlgorithm[TypeEnvironment, Statement](TypeTransferFunction(true, false), bottomEnv, baseEnv, cfg)

        aa.init
        aa.computeFixpoint

        if (Main.displayFixPoint) {
            println("     - Fixpoint:");
            for ((v,e) <- aa.getResult.toList.sortWith{(x,y) => x._1.name < y._1.name}) {
                println("      * ["+v+"] => "+e);
            }
        }

        // Detect unreachables:
        for (l <- aa.detectUnreachable(TypeTransferFunction(true, false))) {
            Reporter.notice("Unreachable code", l)
        }

        // Collect errors and annotations
        aa.pass(TypeTransferFunction(false, !Main.exportAPIPath.isEmpty))

        // Collect retvals
        scope match {
            case fs: FunctionSymbol =>
                // collect return value
                val facts = aa.getResult;
                val retType = facts(cfg.exit).map.getOrElse(TempID("retval"), TBottom);

                AnnotationsStore.collectFunctionRet(fs, retType)
            case _ =>
        }
    }
}
