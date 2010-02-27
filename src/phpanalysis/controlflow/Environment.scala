package phpanalysis.controlflow

abstract class Environment[E <: Environment[_]] {
    def union (env: E): E;
}
