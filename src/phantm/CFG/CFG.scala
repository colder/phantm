package phantm.CFG

import Trees.CFGStatement

class CFG extends LabeledDirectedGraphImp[CFGStatement] {
  val entry: Vertex = newVertex
  val exit: Vertex = newVertex
  entry.name = "entry"
  exit.name = "exit"
  override def toString = "[>" + entry + super.toString + exit + "<]"
}
