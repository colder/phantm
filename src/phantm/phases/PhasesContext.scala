package phantm.phases

import phantm.ast.Trees.Program
import phantm.util.Unserializer

case class PhasesContext(
    val files: List[String] = Nil,
    val oast: Option[Program] = None,
    val dumpedData: List[Unserializer] = Nil
);
