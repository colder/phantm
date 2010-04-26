package phantm.phases

import phantm.Main
import phantm.util.{ConstantsResolver, IncludeResolver}

object IncludesConstantsResolutionPhase extends Phase(Some(SymbolsCollectionPhase)) {
    def name = "Resolving includes"
    def description = "Resolving includes and constants"

    def run(ctx: PhasesContext): PhasesContext = {
        var newCtx = ctx

        newCtx = newCtx.setAST(ConstantsResolver(newCtx.oast.get, false).transform)
        newCtx = newCtx.setAST(IncludeResolver(newCtx.oast.get).transform)
        newCtx = newCtx.setAST(ConstantsResolver(newCtx.oast.get, false).transform)

        if (Main.displayIncludes) {
            println("     - Files sucessfully imported:")
            for (f <- IncludeResolver.includedFiles) {
                println("       * "+f)
            }
        }

        newCtx
    }
}
