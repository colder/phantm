package phantm.phases

abstract class Phase {
    def name: String
    def description: String

    def run(ctx: PhasesContext): PhasesContext

    override def toString = name
}

case class PhaseException(ph: Phase, error: String) extends Exception
