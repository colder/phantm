package phantm.controlflow

abstract class TransferFunction[E, S] {
  def apply(node : S, x : E) : E
}
