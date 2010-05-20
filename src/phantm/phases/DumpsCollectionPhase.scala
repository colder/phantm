package phantm.phases

import io.Source
import java.io.File
import phantm.Settings
import phantm.util.DumpCollector

object DumpsCollectionPhase extends Phase(Some(TypeAnalyzingPhase)) {

    def name = "Dumps collections"
    def description = "Collecting and importing dumps"

    def run(ctx: PhasesContext): PhasesContext = {
        var data = List[DumpCollector]()

        if (Settings.get.dumps != Nil) {
            for (path <- Settings.get.dumps) {
                data = new DumpCollector(path) :: data
            }
        }

        ctx.copy(dumpedData = data)
    }

}
