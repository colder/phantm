package phantm.phases

import phantm.ast.Trees.Program
import phantm.util.{DumpCollector, Positional}
import phantm.symbols.Symbol
import phantm.types.Type
import phantm.symbols.FunctionSymbol
import phantm.cfg.ControlFlowGraph

case class PhasesContext(
    val files: List[String] = Nil,
    val oast: Option[Program] = None,
    val dumpedData: List[DumpCollector] = Nil,
    val symbol: Option[Symbol] = None,
    val cfgs: Map[Option[FunctionSymbol], ControlFlowGraph] = Map(),
    var inlineCache: Map[FunctionSymbol, Map[List[Type], Type]] = Map().withDefaultValue(Map()),
    var globalCalls: Map[FunctionSymbol, Map[String, Type]] = Map().withDefaultValue(Map()),
    var reachableFromMain: Map[FunctionSymbol, Set[FunctionSymbol]] = Map().withDefaultValue(Set())
);
