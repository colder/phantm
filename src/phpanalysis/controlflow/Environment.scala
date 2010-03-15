package phpanalysis.controlflow

abstract class Environment[E <: Environment[_, S],S] {
    def union (env: E): E;
    def copy: E;
    def checkMonotonicity (newEnv: E, inEdges: Iterable[(S, E)]): Unit;
}
