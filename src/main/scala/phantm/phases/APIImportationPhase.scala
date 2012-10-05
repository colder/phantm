package phantm.phases

import phantm.Settings
import phantm.util.{API, Reporter}
import java.io.File

object APIImportationPhase extends Phase {
    def name = "API importation"
    def description = "Importing API from XML files"

    def run(ctx: PhasesContext): PhasesContext = {
        if (Settings.get.importAPI) {
            var internal_api = getClass().getClassLoader().getResourceAsStream("spec/internal_api.xml")

            if (internal_api == null) {
              // Second approach, if not run from jar
              internal_api = new java.io.FileInputStream("spec/internal_api.xml")
            }

            new API.Reader(internal_api, ctx).load

            for (api <- Settings.get.apis){
                if (new File(api).exists()) {
                    new API.Reader(api, ctx).load
                } else {
                    Reporter.get.error("Failed to load API file '"+api+"': File not found");
                }
            }
        }
        ctx
    }
}
