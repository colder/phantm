package phantm.analyzer

import Symbols.GlobalSymbols
import Types._
import phantm.AST.Trees._
import java.io.File

object Evaluator {

    def typeFromExpr(oe: Option[Expression]): Type = oe match {
        case Some(e) => typeFromExpr(e)
        case None => TNull
    }

    def typeFromExpr(e: Expression): Type = e match {
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

    def scalarToString(ex: Scalar) = ex match {
        case PHPTrue() =>
            "1"
        case PHPFalse() =>
            ""
        case PHPInteger(value) =>
            ""+value
        case PHPFloat(value) =>
            ""+value
        case PHPString(value) =>
            value
        case PHPNull() =>
            ""

        case MCFile() =>
            ex.file match {
                case Some(p) =>
                    new File(p).getAbsolutePath()
                case None =>
                    "internal"
            }
        case MCLine() =>
            ""+ex.line
        case MCDir() =>
            ex.file match {
                case Some(p) =>
                    dirname(new File(p).getAbsolutePath())
                case None =>
                    "internal"
            }
    }

    def staticEval(ex: Expression): Option[Scalar] = staticEval(ex, true)

    def staticEval(ex: Expression, issueErrors: Boolean): Option[Scalar] = ex match {
        case Concat (lhs, rhs) =>
            (staticEval(lhs, issueErrors), staticEval(rhs, issueErrors)) match {
                case (Some(slhs), Some(srhs)) => Some(PHPString(scalarToString(slhs)+scalarToString(srhs)).setPos(slhs))
                case _ => None
            }
        case FunctionCall(StaticFunctionRef(_,_,Identifier("dirname")), List(CallArg(arg, _))) =>
            staticEval(arg, issueErrors) match {
                case Some(a) =>
                    Some(PHPString(dirname(scalarToString(a))).setPos(ex))
                case None =>
                    None
            }
        case Constant(name) =>
            GlobalSymbols.lookupOrRegisterConstant(name).value
        case ClassConstant(_:StaticClassRef, _) =>
            Some(PHPString("CLASSCONSTANT").setPos(ex))
        case sc: Scalar =>
            Some(sc)
        case _ =>
            None
    }

    def dirname(path: String): String = {
        val ind = path.lastIndexOf('/')

        if (ind < 0) {
            "."
        } else {
            path.substring(0, ind)
        }
    }

}
