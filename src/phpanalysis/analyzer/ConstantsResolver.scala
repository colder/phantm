package phpanalysis.analyzer
import analyzer.Symbols._
import analyzer.Types._
import parser.Trees._
import scala.collection.mutable.Map;

case class ConstantsResolver(ast: Program, issueErrors: Boolean) extends ASTTransform(ast) {

    override def trExpr(ex: Expression): Expression = ex match {
        case FunctionCall(StaticFunctionRef(_, _, Identifier("define")), List(CallArg(PHPString(name), _), CallArg(expr, _))) =>
            GlobalSymbols.lookupConstant(name) match {
                case None =>
                    Evaluator.staticEval(expr) match {
                        case Some(v) =>
                            val cs = new ConstantSymbol(name, Some(v), v match {
                                case PHPTrue() => TBoolean
                                case PHPFalse() => TBoolean
                                case PHPInteger(_) => TInt
                                case PHPFloat(_) => TFloat
                                case PHPString(_) => TString
                                case PHPNull() => TNull
                                case MCLine() => TInt
                                case _ => TString
                            })

                            GlobalSymbols.registerConstant(cs)
                        case None =>
                            if (issueErrors) {
                                Reporter.notice("Dynamic constant declaration ignored", expr)
                            }
                    }
                case Some(_) =>
                    // already defined: ignore
            }
            ex

        case FunctionCall(StaticFunctionRef(_, _, Identifier("define")), _) =>
            if (issueErrors) {
                Reporter.notice("Dynamic constant declaration ignored", ex)
            }
            ex

        case _ => super.trExpr(ex)
    }

}
