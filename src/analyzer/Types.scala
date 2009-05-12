package phpanalysis.analyzer;
import Symbols._


object Types {
    sealed abstract class Type {
        def isSubTypeOf(tpe: Type): Boolean = this == tpe
        def allowTypeCastTo(tpe: Type): Boolean = true
    }


    case object TInt extends Type
    case object TBoolean extends Type
    case object TFloat extends Type
    case object TString extends Type
    case object TArray extends Type
    case object TResource extends Type
    case object TNull extends Type
    case class TObject(cl: ClassSymbol) extends Type
    case object TAnyObject extends Type

    case object TMixed extends Type {
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
