package phantm.phases

import phantm.Settings
import phantm.util.{Reporter, ErrorException}

class PhasesRunner(val reporter: Reporter) {
    def run(initCtx: PhasesContext) = {
        var ctx = initCtx

        var oph: Option[Phase] = Some(DumpsCollectionPhase)

        var i = 1;
        while(oph != None) {
            val ph = oph.get
            try {
                if (Settings.get.displayProgress) {
                    println(i+": "+ph.name+"...")
                }
                ctx = ph.run(ctx)
                oph = ph.next
                i += 1
            } catch {
                case e: PhaseException =>
                    reporter.error("Processing failed at phase "+i+" ("+e.ph.name+"): "+e.error)
                    oph = None
            }
        }

        reporter.emitAll

        val ec  = reporter.errorsCount
        val tec = reporter.totalErrorsCount
        val nc  = reporter.noticesCount
        val tnc = reporter.totalNoticesCount

        if (Settings.get.focusOnMainFiles) {
            println(nc+" notice"+(if (nc>1) "s" else "")+" and "+ec+" error"+(if (ec>1) "s" else "")+" occured in main files, abort.")
            println(tnc+" notice"+(if (tnc>1) "s" else "")+" and "+tec+" error"+(if (tec>1) "s" else "")+" occured in total.")
        } else {
            println(nc+" notice"+(if (nc>1) "s" else "")+" and "+ec+" error"+(if (ec>1) "s" else "")+" occured, abort.")

        }
    }
}
