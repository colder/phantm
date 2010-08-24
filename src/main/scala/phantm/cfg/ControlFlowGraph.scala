package phantm.cfg

import Trees.Statement

class ControlFlowGraph extends LabeledDirectedGraphImp[Statement] {
  val entry: Vertex = newVertex
  val exit: Vertex = newVertex
  entry.name = "entry"
  exit.name = "exit"
  override def toString = "[>" + entry + super.toString + exit + "<]"
}
