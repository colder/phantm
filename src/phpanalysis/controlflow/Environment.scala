package phpanalysis.controlflow

abstract class Environment[E <: Environment[_, S],S] {
    type Vertex = VertexImp[S]

    def union (env: E): E;
    def copy: E;
    def checkMonotonicity (vertex: Vertex, newEnv: E, inEdges: Iterable[(S, E)]): Unit;
}
