package phpanalysis.controlflow
 
abstract class TransferFunction[E, S] {
  def apply(node : S, x : E) : E
}

abstract class Environment[E <: Environment[_]] {
    def union (env: E): E;
    def equals (env: E): Boolean;
}

class AnalysisAlgoritm[E <: Environment[E],S]
               (transferFun : TransferFunction[E,S],
		baseEnv : E,
		cfg : LabeledDirectedGraphImp[S])
{
  type Vertex = VertexImp[S]
  
  var facts : Map[Vertex, E] = Map[Vertex,E]()
  
  def init = {
    facts = Map[Vertex,E]().withDefaultValue(baseEnv)
  }
  
  def pass(transferFun: TransferFunction[E,S]) = {
      for (v <- cfg.V) {
        for (e <- cfg.inEdges(v)) {
          transferFun(e.lab, facts(e.v1))
        }
      }
  }

  def computeFixpoint : Unit = {
    var change = true
    while (change) {
      change = false
      for (v <- cfg.V) {
        val oldFact : E = facts(v)
        var newFact : E = oldFact
        for (e <- cfg.inEdges(v)) {
          val propagated = transferFun(e.lab, facts(e.v1))
          newFact = newFact union propagated
        }
        if (!(newFact equals oldFact)) {
          change = true;
          facts = facts.update(v, newFact)
        }
      }
    }
  }
  def getResult : Map[Vertex,E] = facts
}
