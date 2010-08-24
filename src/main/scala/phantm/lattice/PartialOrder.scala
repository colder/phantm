package phantm.lattice

trait PartialOrder {
    type Env
    type E
    def leq(envx: Env, envy: Env, x : E, y : E) : Boolean
}
