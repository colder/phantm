package phpanalysis.controlflow
import scala.collection.Set

trait PartialOrder[UData] {
    type E
    def leq(ex: UData, x : E, y : E) : Boolean = leq(ex, ex, x, y)
    def leq(ex: UData, ey: UData, x : E, y : E) : Boolean // less than or equal
}
trait Lattice[UData] extends PartialOrder[UData] {
    val top : E                  // universal set
    val bottom : E               // empty set
    def join(x : E, y : E) : E   // union
    def meet(x : E, y : E) : E   // intersection
}
