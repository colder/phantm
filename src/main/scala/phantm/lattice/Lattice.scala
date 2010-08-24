package phantm.lattice

trait Lattice extends PartialOrder {
    val top : E                  // universal set
    val bottom : E               // empty set
    def join(env: Env, x : E, y : E) : (Env, E)   // union
    def meet(env: Env, x : E, y : E) : (Env, E)   // intersection
}
