package phantm.phases

import phantm.Settings
import phantm.symbols._
import phantm.ast.{Trees => AST}
import phantm.util.{ConstantsResolver, IncludeResolver, Evaluator}
import phantm.types.TypeHelpers

object IncludesConstantsResolutionPhase extends Phase {
    def name = "Resolving includes"
    def description = "Resolving includes and constants"

    def run(ctx: PhasesContext): PhasesContext = {
        var ast: AST.Program = ctx.oast.get

        // We register all constants found in dumped state
        val consts = ctx.dumpedData.flatMap(d => d.constants.toScalarMap).toMap
        for ((name, expr) <- consts if GlobalSymbols.lookupConstant(name) == None) {
            Evaluator.staticEval(expr, false) match {
                case Some(v) =>
                    val cs = new ConstantSymbol(name, Some(v))
                    cs.typ = TypeHelpers.exprToType(v)

                    GlobalSymbols.registerConstant(cs)
                case _ =>
                    error("Unnexpected non-evaluable scalar value")
            }
        }

        // We register every files as included already

        for (f <- ctx.dumpedData.flatMap(d => d.files)) {
            IncludeResolver.includedFiles += f
        }

        ast = ConstantsResolver(ast, false).transform
        ast = IncludeResolver(ast).transform
        ast = ConstantsResolver(ast, false).transform

        if (Settings.get.displayIncludes) {
            println("     - Files sucessfully imported:")
            for (f <- IncludeResolver.includedFiles) {
                println("       * "+f)
            }
        }

        ctx.copy(oast = Some(ast))
    }
}
