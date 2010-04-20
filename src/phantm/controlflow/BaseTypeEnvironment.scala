package phantm.controlflow

import phantm.analyzer.Types.Type
import phantm.analyzer.Types.ObjectStore
import phantm.CFG.Trees.SimpleVariable

object BaseTypeEnvironment extends TypeEnvironment(Map[SimpleVariable, Type](), None, new ObjectStore) {
    override def union(e: TypeEnvironment) = {
        e
    }

    override def equals(e: Any) = {
        if (e.isInstanceOf[AnyRef]) {
            BaseTypeEnvironment eq e.asInstanceOf[AnyRef]
        } else {
            false
        }
    }

    override def copy: TypeEnvironment =
        this

    override def toString = {
        "<base>"
    }

}
