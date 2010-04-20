package phantm.phases

import phantm.ast.Trees.Program

class PhasesContext(
            val files: List[String],
            val oast: Option[Program]
            ) {

    def this() = {
        this(Nil, None)
    }

    def setFiles(files: List[String]) = new PhasesContext(files, oast)
    def setAST(ast: Program) = new PhasesContext(files, Some(ast))

}
