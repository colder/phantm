package phantm.phases

import phantm.Main
import phantm.util.Unserializer

object DumpsCollectionPhase extends Phase(Some(TypeAnalyzingPhase)) {

    def name = "Dumps collections"
    def description = "Collecting and importing dumps"

    def run(ctx: PhasesContext): PhasesContext = {
        if (ctx.settings.dumps != Nil) {
            for (dump <- ctx.settings.dumps) {
                Main.dumpedData = Unserializer.fromDump(dump) :: Main.dumpedData
            }
        }

        ctx
    }

}
