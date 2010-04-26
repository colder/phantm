package phantm.phases

import phantm.util.{API, Reporter}

object APIExportingPhase extends Phase(None) {
    def name = "API exportation"
    def description = "exporting API to XML files"

    def run(reporter: Reporter, ctx: PhasesContext): PhasesContext = {
        if (!ctx.settings.exportAPIPath.isEmpty) {
            new API.Writer(ctx.settings.exportAPIPath.get).emitXML
        }
        ctx
    }
}
