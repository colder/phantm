package phpanalysis.controlflow
import scala.collection.Set

trait PartialOrder {
    type E
    def leq(x : E, y : E) : Boolean
}
trait Lattice[Env] extends PartialOrder {
    val top : E                  // universal set
    val bottom : E               // empty set
    def join(x : E, y : E) : E   // union
    def meet(env: Env, x : E, y : E) : (Env, E)   // intersection
}
