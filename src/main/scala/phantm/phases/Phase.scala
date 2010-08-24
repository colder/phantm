package phantm.phases

abstract class Phase {
    def name: String
    def description: String

    def run(ctx: PhasesContext): PhasesContext

    override def toString = name

    def andThen(phase: Phase) = PhaseSeq() andThen this andThen phase
}

case class PhaseException(ph: Phase, error: String) extends Exception
