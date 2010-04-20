package phantm.phases

import phantm.Main
import phantm.util.API

object APIImportationPhase extends Phase(Some(IncludesConstantsResolutionPhase)) {
    def name = "API importation"
    def description = "Importing API from XML files"

    def run(ctx: PhasesContext): PhasesContext = {
        if (Main.importAPI) {
            new API.Reader(Main.mainDir+"spec/internal_api.xml").load

            for (api <- Main.apis) {
                new API.Reader(api).load
            }
        }
        ctx
    }
}
