package phpanalysis.controlflow
 
import scala.collection.mutable.HashSet;

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
      // We should traverse the graph in order
      var toProcess = List(cfg.entry)
      var processed = HashSet[Vertex]();

      while (toProcess != Nil) {
        val v = toProcess.head
        toProcess = toProcess.tail

        processed += v

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

        for (e <- cfg.outEdges(v)) {
            if (!(processed contains e.v2)) {
                toProcess = toProcess ::: e.v2 :: Nil
            }
        }
      }
    }
  }
  def getResult : Map[Vertex,E] = facts
}
