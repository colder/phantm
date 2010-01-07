package phpanalysis.analyzer
import parser.Trees._

case class Annotations(ast: Program) extends ASTTransform(ast) {
    override def trStmts(stmts: List[Statement]): List[Statement] = stmts match {
        case DocComment(comment) :: (fd: FunctionDecl) :: sts =>
            trStmt(importFunctionsAnnotations(fd, comment)) :: trStmts(sts)
        case ts :: sts =>
            trStmt(ts) :: trStmts(sts)
        case Nil =>
            Nil

    }

    def importFunctionsAnnotations(fd: FunctionDecl, comment: String): FunctionDecl = {
        // Here we import the annotations as type hints for functions
        // TODO: Parse the comment and inject comments
        fd
    }

    def importMethodsAnnotations(md: MethodDecl, comment: String): MethodDecl = {
        // Here we import the annotations as type hints for methods
        // TODO: Parse the comment and inject comments
        md
    }

    def importPropertiesAnnotations(pd: PropertyDecl, comment: String): PropertyDecl = {
        // Here we import the annotations as type hints for properties
        // TODO: Parse the comment and inject comments
        pd
    }
}
