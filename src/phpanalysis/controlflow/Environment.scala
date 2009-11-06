package phpanalysis.controlflow

abstract class Environment[E <: Environment[_]] {
    def union (env: E): E;
    def equals (env: E): Boolean;
}
