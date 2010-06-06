package phantm.phases

import phantm.ast.Trees.Program
import phantm.util.DumpCollector
import phantm.symbols.Symbol
import phantm.types.Type
import phantm.symbols.FunctionSymbol
import phantm.cfg.ControlFlowGraph

case class PhasesContext(
    val files: List[String] = Nil,
    val oast: Option[Program] = None,
    val dumpedData: List[DumpCollector] = Nil,
    val symbol: Option[Symbol] = None,
    val globals: Option[Type] = None,
    val cfgs: Map[Option[FunctionSymbol], ControlFlowGraph] = Map[Option[FunctionSymbol], ControlFlowGraph]()
);
