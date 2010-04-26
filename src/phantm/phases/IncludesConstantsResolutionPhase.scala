package phantm.phases

import phantm.Settings
import phantm.ast.{Trees => AST}
import phantm.util.{ConstantsResolver, IncludeResolver}

object IncludesConstantsResolutionPhase extends Phase(Some(SymbolsCollectionPhase)) {
    def name = "Resolving includes"
    def description = "Resolving includes and constants"

    def run(ctx: PhasesContext): PhasesContext = {
        var ast: AST.Program = ctx.oast.get

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
