package phantm.phases

import phantm.Settings
import phantm.util._
import scala.util.control.Breaks._

class PhasesRunner(val reporter: Reporter) {
    def getPhasesToRun: PhaseSeq = (
        DumpsCollectionPhase
        followedBy APIImportationPhase
        followedBy ParsingPhase
        followedBy ASTPruningPhase
        followedBy IncludesConstantsResolutionPhase
        followedBy ASTChecksPhase
        followedBy SymbolsCollectionPhase
        followedBy SymbolsChecksPhase
        followedBy PureStatementsPhase
        followedBy CallGraphPhase
        followedBy MethodGraphPhase
        followedBy CFGGenerationPhase
        followedBy TypeAnalyzingPhase
        followedBy APIExportingPhase
    )

    def run(initCtx: PhasesContext) = {
        var ctx = initCtx

        val phases = getPhasesToRun.list

        breakable {
            for((ph, i) <- phases.zipWithIndex) {
                try {
                    if (Settings.get.displayProgress) {
                        println((i+1)+": "+ph.name+"...")
                    }
                    ctx = ph.run(ctx)
                } catch {
                    case e: PhaseException =>
                        reporter.error("Processing failed at phase "+(i+1)+" ("+e.ph.name+"): "+e.error)
                        break;
                }
            }
        }

        if (!Settings.get.summaryOnly) {
            reporter.emitAll
        }

        reporter.emitSummary

        reporter.clear
    }
}
