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
        for ((name, expr) <- consts if ctx.globalSymbols.lookupConstant(name) == None) {
            Evaluator.staticEval(expr, ctx, false) match {
                case Some(v) =>
                    val cs = new ConstantSymbol(name, Some(v))
                    cs.typ = TypeHelpers.exprToType(v)

                    ctx.globalSymbols.registerConstant(cs)
                case _ =>
                    sys.error("Unnexpected non-evaluable scalar value")
            }
        }

        // We register every files as included already

        for (f <- ctx.dumpedData.flatMap(d => d.files)) {
            IncludeResolver.includedFiles += f
        }

        ast = ConstantsResolver(ast, false, ctx).transform
        ast = IncludeResolver(ast, ctx).transform
        ast = ConstantsResolver(ast, false, ctx).transform

        if (Settings.get.displayIncludes) {
            println("     - Files sucessfully imported:")
            for (f <- IncludeResolver.includedFiles) {
                println("       * "+f)
            }
        }

        ctx.copy(oast = Some(ast))
    }
}
