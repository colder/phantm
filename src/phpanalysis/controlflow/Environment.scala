package phpanalysis.controlflow

abstract class Environment[E <: Environment[_]] {
    def union (env: E): E;
    def copy: E;
    def dumpDiff (env: E): Unit;
    def checkMonotonicity (newEnv: E): Boolean;
}
