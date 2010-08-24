package phantm.phases

import phantm.parser.Parser
import phantm.ast.STToAST

object ParsingPhase extends Phase {
    def name = "Parsing"
    def description = "Generating AST"

    def run(ctx: PhasesContext): PhasesContext = {

        val sts = ctx.files map { f => val c = new Parser(f); (f, c, c parse) }

        val er = sts find { _._3 == None };

        if (er != None) {
            val e = er.get
            throw PhaseException(this, "Parsing  of '"+e._1+"' failed")
        }

        val asts = sts map { c => new STToAST(c._2, c._3.get) getAST }

        val ast = asts.reduceLeft {(a,b) => a combine b}

        ctx.copy(oast = Some(ast))
    }
}
