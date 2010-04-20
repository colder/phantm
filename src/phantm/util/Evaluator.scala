package phantm.util

import phantm.symbols.GlobalSymbols
import phantm.types._
import phantm.ast.Trees._
import java.io.File

object Evaluator {

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
