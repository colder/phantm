package phantm.phases

import phantm.ast.Trees.Program
import phantm.util._
import phantm.symbols.{Symbol,GlobalSymbols}
import phantm.types.{Type, TypeEnvironment, ObjectStore}
import phantm.symbols.FunctionSymbol
import phantm.cfg.ControlFlowGraph

case class PhasesContext(
    val files: List[String] = Nil,
    val oast: Option[Program] = None,
    val dumpedData: List[DumpCollector] = Nil,
    val symbol: Option[Symbol] = None,
    val results: GlobalAnalysisResults = new GlobalAnalysisResults,
    val cfgs: Map[Option[FunctionSymbol], ControlFlowGraph] = Map(),
    val globalSymbols: GlobalSymbols = new GlobalSymbols
);
