package phantm.phases

import phantm.Settings
import phantm.util.{API, Reporter}

object APIExportingPhase extends Phase {
    def name = "API exportation"
    def description = "exporting API to XML files"

    def run(ctx: PhasesContext): PhasesContext = {
        if (!Settings.get.exportAPIPath.isEmpty) {
            new API.Writer(Settings.get.exportAPIPath.get, ctx).emitXML
        }
        ctx
    }
}
