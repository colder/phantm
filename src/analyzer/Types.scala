package phpanalysis.analyzer;
import Symbols._


object Types {
    sealed abstract class Type

    sealed abstract class ClassType
    object TClassAny extends ClassType
    case class TClass(cd: ClassSymbol) extends ClassType
    case class TClassUnion(l: ClassType, r: ClassType) extends ClassType


    object TInt extends Type
    object TBoolean extends Type
    object TFloat extends Type
    object TString extends Type

    object TResource extends Type
    object TNull extends Type

    case class TUnion(l: Type, r: Type) extends Type {
        
    }

    case class TObject(cl: ClassType) extends Type
    case class TArray(valuesType: Type) extends Type

    object TAnyObject extends Type

    object TAny extends Type

    trait Typed {
        self =>

        private var _tpe: Type = TAny

        def setType(tpe: Type): self.type = { _tpe = tpe; this }
        def getType: Type = _tpe
    }

}
