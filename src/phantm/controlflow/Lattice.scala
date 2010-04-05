package phpanalysis.controlflow
import scala.collection.Set

trait PartialOrder {
    type Env
    type E
    def leq(envx: Env, envy: Env, x : E, y : E) : Boolean
}
trait Lattice extends PartialOrder {
    val top : E                  // universal set
    val bottom : E               // empty set
    def join(envx: Env, envy: Env, x : E, y : E) : (Env, E)   // union
    def meet(envx: Env, envy: Env, x : E, y : E) : (Env, E)   // intersection
}
