package phantm.phases

import phantm.util.{Reporter, ErrorException}

class PhasesRunner(val reporter: Reporter) {
    def run(initCtx: PhasesContext) = {
        val settings = initCtx.settings

        try {
            var ctx = initCtx

            var oph: Option[Phase] = Some(CompilationPhase)

            var i = 1;
            while(oph != None) {
                val ph = oph.get
                try {
                    if (settings.displayProgress) {
                        println(i+": "+ph.name+"...")
                    }
                    ctx = ph.run(ctx)
                    reporter.errorMilestone
                    oph = ph.next
                    i += 1
                } catch {
                    case e: PhaseException =>
                        reporter.error("Processing failed at phase "+i+" ("+e.ph.name+"): "+e.error)
                        reporter.errorMilestone
                }
            }
            val n = reporter.getNoticesCount
            val tn = reporter.getTotalNoticesCount

            if (settings.focusOnMainFiles && n > 0 && tn > n) {
                println(n+" notice"+(if (n>1) "s" else "")+" occured in main files.")
                println(tn+" notice"+(if (tn>1) "s" else "")+" occured in total.")
            } else {
                println(n+" notice"+(if (n>1) "s" else "")+" occured.")
            }

        } catch {
            case ErrorException(en, nn, etn, ntn) =>
                if (settings.focusOnMainFiles) {
                    println(nn+" notice"+(if (nn>1) "s" else "")+" and "+en+" error"+(if (en>1) "s" else "")+" occured in main files, abort.")
                    println(ntn+" notice"+(if (ntn>1) "s" else "")+" and "+etn+" error"+(if (etn>1) "s" else "")+" occured in total.")
                } else {
                    println(nn+" notice"+(if (nn>1) "s" else "")+" and "+en+" error"+(if (en>1) "s" else "")+" occured, abort.")

                }
        }
    }
}
