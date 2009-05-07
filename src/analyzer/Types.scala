package phpanalysis.analyzer;


object Types {
    sealed abstract class Type {
        def isSubTypeOf(tpe: Type): Boolean
        def isSameTypeAs(tpe: Type): Boolean = this == tpe
    }


    case object TInt extends Type {
        def isSubTypeOf(tpe: Type): Boolean = tpe match {
            case TInt => true
            case _ => false
        }
    }


}
