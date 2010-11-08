package phantm.phases

import phantm.Settings
import phantm.util.{API, Reporter}
import java.io.File

object APIImportationPhase extends Phase {
    def name = "API importation"
    def description = "Importing API from XML files"

    def run(ctx: PhasesContext): PhasesContext = {
        if (Settings.get.importAPI) {
            new API.Reader(getClass().getClassLoader().getResourceAsStream("spec/internal_api.xml")).load

            for (api <- Settings.get.apis){
                if (new File(api).exists()) {
                    new API.Reader(api).load
                } else {
                    Reporter.get.error("Failed to load API file '"+api+"': File not found");
                }
            }
        }
        ctx
    }
}
