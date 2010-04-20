package phantm.types

import phantm.AST.Trees._

object TypeHelpers {
    def exprToType(oe: Option[Expression]): Type = oe match {
        case Some(e) => exprToType(e)
        case None => TNull
    }

    def exprToType(e: Expression): Type = e match {
        case PHPTrue() => TBoolean
        case PHPFalse() => TBoolean
        case PHPInteger(i) => TIntLit(i)
        case PHPFloat(f) => TFloatLit(f)
        case PHPString(s) => TStringLit(s)
        case PHPNull() => TNull
        case MCFile() => TString
        case MCLine() => TString
        case MCDir() => TString
        case MCClass() => TString
        case MCFunction()  => TString
        case MCMethod() => TString
        case MCNamespace() => TString
        case Minus(_, _) => TInt
        case a: Array =>
            //TODO
            TAnyArray
        case _=>
            TAny
    }


    def typeHintToType(th: Option[TypeHint]): Type = th match {
        case None => TAny
        case Some(th) => typeHintToType(th)
    }

    def typeHintToType(th: TypeHint): Type = th match {
        case THArray =>
            TAnyArray
        case THObject(cl: ClassRef) =>
            // TODO
            TAnyObject
    }
}
