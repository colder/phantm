package phpanalysis.analyzer

import analyzer.Symbols.GlobalSymbols
import parser.Trees._
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

    def staticEval(ex: Expression): Option[Scalar] = ex match {
        case Concat (lhs, rhs) =>
            (staticEval(lhs), staticEval(rhs)) match {
                case (Some(slhs), Some(srhs)) => Some(PHPString(scalarToString(slhs)+scalarToString(srhs)).setPos(slhs))
                case _ => None
            }
        case FunctionCall(StaticFunctionRef(_,_,Identifier("dirname")), List(CallArg(arg, _))) =>
            staticEval(arg) match {
                case Some(a) =>
                    Some(PHPString(dirname(scalarToString(a))).setPos(ex))
                case None =>
                    None
            }
        case Constant(name) =>
            GlobalSymbols.lookupConstant(name.value) match {
                case Some(cs) =>
                    cs.value
                case None =>
                    Reporter.notice("Potentially undefined constant '"+name.value+"'", ex)
                    Some(PHPString(name.value).setPos(ex))
            }
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
