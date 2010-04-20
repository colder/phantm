package phantm.phases

import phantm.Main
import phantm.util.{ConstantsResolver, IncludeResolver, Reporter}

object IncludesConstantsResolutionPhase extends Phase(Some(SymbolsCollectionPhase)) {
    def name = "Resolving includes"
    def description = "Resolving includes and constants"

    def run(ctx: PhasesContext): PhasesContext = {
        var newCtx = ctx

        newCtx = newCtx.setAST(ConstantsResolver(newCtx.oast.get, false).transform)
        Reporter.errorMilestone

        newCtx = newCtx.setAST(IncludeResolver(newCtx.oast.get).transform)
        Reporter.errorMilestone

        newCtx = newCtx.setAST(ConstantsResolver(newCtx.oast.get, false).transform)
        Reporter.errorMilestone

        if (Main.displayIncludes) {
            println("     - Files sucessfully imported:")
            for (f <- IncludeResolver.includedFiles) {
                println("       * "+f)
            }
        }

        newCtx
    }
}
