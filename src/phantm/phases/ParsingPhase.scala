package phantm.phases

import phantm.parser.Parser
import phantm.ast.STToAST

object ParsingPhase extends Phase {
    def name = "Parsing"
    def description = "Generating AST"

    def run(ctx: PhasesContext): PhasesContext = {

        val sts = ctx.files map { f => val c = new Parser(f); (c, c parse) }

        if (sts exists { _._2 == None} ) {
            throw PhaseException(this, "Parsing failed")
        }

        val asts = sts map { c => new STToAST(c._1, c._2.get) getAST }

        val ast = asts.reduceLeft {(a,b) => a combine b}

        ctx.copy(oast = Some(ast))
    }
}
