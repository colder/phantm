package phantm.phases

import phantm.Compiler
import phantm.ast.STToAST



object CompilationPhase extends Phase(Some(APIImportationPhase)) {
    def name = "Compilation"
    def description = "Generating AST"

    def run(ctx: PhasesContext): PhasesContext = {
        val sts = ctx.files map { f => val c = new Compiler(f); (c, c compile) }

        if (sts exists { _._2 == None} ) {
            throw PhaseException(this, "Compilation failed")
        }

        val asts = sts map { c => new STToAST(c._1, c._2.get) getAST }

        ctx.setAST(asts.reduceLeft {(a,b) => a combine b})
    }
}
