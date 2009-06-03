package phpanalysis.analyzer;
import Symbols._


object Types {
    sealed abstract class Type {
        def isSubTypeOf(tpe: Type): Boolean = this == tpe
        def allowTypeCastTo(tpe: Type): Boolean = true
    }


    object TInt extends Type
    object TBoolean extends Type
    object TFloat extends Type
    object TString extends Type
    object TArray extends Type
    object TResource extends Type
    object TNull extends Type
    case class TObject(cl: ClassSymbol) extends Type
    object TAnyObject extends Type

    object TMixed extends Type {
        override def isSubTypeOf(tpe: Type): Boolean = true
        override def allowTypeCastTo(tpe: Type): Boolean = true
    }

    trait Typed {
        self =>

        private var _tpe: Type = TMixed

        def setType(tpe: Type): self.type = { _tpe = tpe; this }
        def getType: Type = _tpe
    }

}
