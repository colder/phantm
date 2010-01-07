package phpanalysis.analyzer
import parser.Trees._

case class Annotations(ast: Program) extends ASTTransform(ast) {
    override def trStmts(stmts: List[Statement]): List[Statement] = stmts match {
        case DocComment(comment) :: (fd: FunctionDecl) :: sts =>
            trStmt(importAnnotations(fd, comment)) :: trStmts(sts)
        case ts :: sts =>
            trStmt(ts) :: trStmts(sts)
        case Nil =>
            Nil

    }

    def importAnnotations(fd: FunctionDecl, comment: String): FunctionDecl = {
        // Here we import the annotations as type hints
        fd
    }
}
