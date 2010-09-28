package phantm.phases

import phantm.Settings
import phantm.ast.Trees._
import phantm.ast.ASTTraversal
import phantm.util.Reporter

object SymbolsChecksPhase extends Phase {

    def name = "Symbols checks"
    def description = "Checking Symbols integrity"

    def run(ctx: PhasesContext): PhasesContext = {
        /*
         * 1) Check that classes not implementing methods are abstract
         */
        // TODO

        /*
         * 2) Check that classes are not implementing conflicting interfaces
         */
        // TODO

        /*
         * 3) Check that class methods match parent/interface prototypes
         */
        // TODO
        ctx
    }

}
