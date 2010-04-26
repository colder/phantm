package phantm.phases

import phantm.util.{API, Reporter}

object APIImportationPhase extends Phase(Some(IncludesConstantsResolutionPhase)) {
    def name = "API importation"
    def description = "Importing API from XML files"

    def run(reporter: Reporter, ctx: PhasesContext): PhasesContext = {
        if (ctx.settings.importAPI) {
            new API.Reader(ctx.settings.mainDir+"spec/internal_api.xml").load

            for (api <- ctx.settings.apis) {
                new API.Reader(api).load
            }
        }
        ctx
    }
}
