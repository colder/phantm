package phantm.controlflow

import phantm.CFG.VertexImp;

abstract class Environment[E <: Environment[_, S],S] {
    type Env = E
    type Vertex = VertexImp[S]

    def union (env: E): E;
    def copy: E;
    def checkMonotonicity (vertex: Vertex, newEnv: E, inEdges: Iterable[(S, E)]): Unit;
}
