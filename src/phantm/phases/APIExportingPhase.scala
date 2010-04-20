package phantm.phases

import phantm.Main
import phantm.util.API

object APIExportingPhase extends Phase(None) {
    def name = "API exportation"
    def description = "exporting API to XML files"

    def run(ctx: PhasesContext): PhasesContext = {
        if (!Main.exportAPIPath.isEmpty) {
            new API.Writer(Main.exportAPIPath.get).emitXML
        }
        ctx
    }
}
