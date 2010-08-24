package phantm.dataflow

import phantm.cfg.VertexImp
import phantm.phases.PhasesContext

abstract class Environment[E <: Environment[_, S],S] {
    type Env = E
    type Vertex = VertexImp[S]

    def union (env: E): E;
    def copy: E;
    def checkMonotonicity (vertex: Vertex, newEnv: E, ctx: PhasesContext, inEdges: Iterable[(S, E)]): Unit;
}
