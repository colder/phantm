package phantm.util

import phantm.symbols.GlobalSymbols
import phantm.types._
import phantm.phases.PhasesContext
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

    def staticEval(ex: Expression, ctx: PhasesContext): Option[Scalar] = staticEval(ex, ctx, true)

    def staticEval(ex: Expression, ctx: PhasesContext, issueErrors: Boolean): Option[Scalar] = ex match {
        case Concat (lhs, rhs) =>
            (staticEval(lhs, ctx, issueErrors), staticEval(rhs, ctx, issueErrors)) match {
                case (Some(slhs), Some(srhs)) => Some(PHPString(scalarToString(slhs)+scalarToString(srhs)).setPos(slhs))
                case _ => None
            }
        case FunctionCall(StaticFunctionRef(nsid), List(CallArg(arg, _))) if nsid.value == "\\dirname" =>
            staticEval(arg, ctx, issueErrors) match {
                case Some(a) =>
                    Some(PHPString(dirname(scalarToString(a))).setPos(ex))
                case None =>
                    None
            }
        case Constant(name) =>
            ctx.globalSymbols.lookupOrRegisterConstant(name).value
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
