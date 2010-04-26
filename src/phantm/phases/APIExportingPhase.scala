package phantm.phases

import phantm.Settings
import phantm.util.{API, Reporter}

object APIExportingPhase extends Phase(None) {
    def name = "API exportation"
    def description = "exporting API to XML files"

    def run(ctx: PhasesContext): PhasesContext = {
        if (!Settings.get.exportAPIPath.isEmpty) {
            new API.Writer(Settings.get.exportAPIPath.get).emitXML
        }
        ctx
    }
}
