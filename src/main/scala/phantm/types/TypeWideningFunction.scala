package phantm.types

abstract class TypeWideningFunction {
    def apply(t: Type): Type
}

object defaultTypeWF extends TypeWideningFunction {
    def apply(t: Type) = t match {
        case TNull => TAny
        case TIntLit(i) => TInt
        case TStringLit(s) => TString
        case TFloatLit(f) => TFloat
        case _ => t
    }
}
