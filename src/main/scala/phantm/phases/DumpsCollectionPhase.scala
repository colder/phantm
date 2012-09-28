package phantm.phases

import io.Source
import java.io.File
import phantm.Settings
import phantm.util.DumpCollector

object DumpsCollectionPhase extends Phase {

    def name = "Dumps collections"
    def description = "Collecting and importing dumps"

    def run(ctx: PhasesContext): PhasesContext = {
        var data = List[DumpCollector]()

        var files = Set[String]()

        if (Settings.get.dumps != Nil) {
            for (path <- Settings.get.dumps) {
                val dc = new DumpCollector(path, ctx)
                data = dc :: data
                files = files ++ dc.files
            }
        }

        files = files -- ctx.files

        ctx.copy(dumpedData = data, files = files.toList ::: ctx.files)
    }

}
