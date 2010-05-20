package phantm.phases

import phantm.ast.Trees.Program
import phantm.util.DumpCollector
import phantm.symbols.Symbol

case class PhasesContext(
    val files: List[String] = Nil,
    val oast: Option[Program] = None,
    val dumpedData: List[DumpCollector] = Nil,
    val symbol: Option[Symbol] = None
);
