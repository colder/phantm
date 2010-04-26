package phantm.util
import phantm.symbols._
import phantm.types.TypeHelpers
import phantm.types.TAny
import phantm.Settings

import phantm.ast.Trees._
import phantm.ast.ASTTransform

import scala.collection.mutable.Map

case class ConstantsResolver(ast: Program, issueErrors: Boolean) extends ASTTransform(ast) {

    override def trExpr(ex: Expression): Expression = ex match {
        case FunctionCall(StaticFunctionRef(_, _, Identifier("define")), List(CallArg(PHPString(name), _), CallArg(expr, _))) =>
            GlobalSymbols.lookupConstant(name) match {
                case None =>
                    Evaluator.staticEval(expr, issueErrors) match {
                        case Some(v) =>
                            val cs = new ConstantSymbol(name, Some(v))
                            cs.typ = TypeHelpers.exprToType(v)

                            GlobalSymbols.registerConstant(cs)
                        case None =>
                            if (issueErrors && Settings.get.verbosity >= 2) {
                                Reporter.notice("Dynamic constant declaration", expr)
                            }

                            if(issueErrors) {
                                val cs = new ConstantSymbol(name, None)
                                GlobalSymbols.registerConstant(cs)
                            }
                    }
                case Some(_) =>
                    // already defined: ignore
            }
            ex

        case FunctionCall(StaticFunctionRef(_, _, Identifier("define")), _) =>
            if (issueErrors && Settings.get.verbosity >= 2) {
                Reporter.notice("Dynamic constant declaration ignored", ex)
            }
            ex

        case _ => super.trExpr(ex)
    }

}
