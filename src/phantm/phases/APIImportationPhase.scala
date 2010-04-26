package phantm.phases

import phantm.Settings
import phantm.util.{API, Reporter}

object APIImportationPhase extends Phase(Some(IncludesConstantsResolutionPhase)) {
    def name = "API importation"
    def description = "Importing API from XML files"

    def run(ctx: PhasesContext): PhasesContext = {
        if (Settings.get.importAPI) {
            new API.Reader(Settings.get.mainDir+"spec/internal_api.xml").load

            for (api <- Settings.get.apis) {
                new API.Reader(api).load
            }
        }
        ctx
    }
}
