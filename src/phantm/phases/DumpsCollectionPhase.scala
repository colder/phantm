package phantm.phases

import phantm.Settings
import phantm.util.Unserializer

object DumpsCollectionPhase extends Phase(Some(TypeAnalyzingPhase)) {

    def name = "Dumps collections"
    def description = "Collecting and importing dumps"

    def run(ctx: PhasesContext): PhasesContext = {
        var data = List[Unserializer]()

        if (Settings.get.dumps != Nil) {
            for (dump <- Settings.get.dumps) {
                data = Unserializer.fromDump(dump) :: data
            }
        }

        ctx.copy(dumpedData = data)
    }

}
